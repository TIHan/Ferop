namespace Ferop

open System
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open Mono.Cecil

open FSharp.Interop.Ferop
open FSharp.Interop.FeropCompiler
open FSharp.Interop.FeropInternal
open FSharp.Interop.FeropInternal.Core

type public WeavingTask () =
    inherit Task ()

    let hasAttribute (typ: Type) (typDef: TypeDefinition) =
        typDef.CustomAttributes
        |> Seq.exists (fun x ->
            x.AttributeType.FullName.Contains(typ.Name))

    let feropClasses (asm: Assembly) =
        asm.GetTypes ()
        |> Array.filter (fun x ->x.IsClass)
        |> Array.filter (fun x ->
            x.CustomAttributes
            |> Seq.exists (fun x -> x.AttributeType = typeof<FeropAttribute>))
        |> List.ofArray

    [<Required>]
    member val AssemblyPath : string = "" with get, set

    [<Required>]
    member val ProjectDirectory : string = "" with get, set

    override this.Execute () : bool =  
        let asmBytes = System.IO.File.ReadAllBytes (this.AssemblyPath)
        let asm = Assembly.Load asmBytes

        let asmDef = AssemblyDefinition.ReadAssembly (this.AssemblyPath)  

        asmDef.Modules
        |> Seq.iter (fun m ->
            m.GetTypes ()
            |> Seq.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
            |> Seq.iter (fun x -> 
                x.Methods
                |> Seq.iter (fun meth ->
                    meth.IsPInvokeImpl <- true
                    meth.IsPreserveSig <- true
                    meth.CallingConvention <- MethodCallingConvention.C
                    m.Import (meth) |> ignore
                )
            )
            m.Write (this.AssemblyPath)
        )

        feropClasses asm
        |> List.iter (fun m ->
            let modul = makeModule m
            C.compileModule this.ProjectDirectory modul Platform.Auto
        )
        true