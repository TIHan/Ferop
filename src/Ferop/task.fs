namespace Ferop

open System
open System.Runtime.CompilerServices
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

    let methodHasAttribute (typ: Type) (methDef: MethodDefinition) =
        methDef.CustomAttributes
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
        let asmDef = AssemblyDefinition.ReadAssembly (this.AssemblyPath)

        asmDef.Modules
        |> Seq.iter (fun m ->
            m.GetTypes ()
            |> Seq.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
            |> Seq.iter (fun x -> 
                x.Methods
                |> Array.ofSeq
                |> Array.iter (fun meth ->
                    if methodHasAttribute typeof<ExportAttribute> meth then
                        let voidType = m.Import(typeof<Void>)
                        let objType = m.Import(typeof<obj>)
                        let nativeintType = m.Import(typeof<nativeint>)
                        let delType = m.Import(typeof<MulticastDelegate>)
                        let compilerGeneratedAttrCtor = m.Import(typeof<CompilerGeneratedAttribute>.GetConstructor(Array.empty))

                        let del = TypeDefinition (meth.DeclaringType.Namespace, meth.Name + "Delegate", TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable, delType)

                        let ctordel = MethodDefinition (".ctor", MethodAttributes.Public, voidType)
                        ctordel.Parameters.Add (ParameterDefinition ("'object'", ParameterAttributes.None, objType))
                        ctordel.Parameters.Add (ParameterDefinition ("'method'", ParameterAttributes.None, nativeintType))
                        ctordel.ImplAttributes <- ctordel.ImplAttributes ||| MethodImplAttributes.Runtime

                        del.Methods.Add (ctordel)

                        let delmeth = MethodDefinition ("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig, meth.ReturnType)
                        delmeth.ImplAttributes <- delmeth.ImplAttributes ||| MethodImplAttributes.Runtime
                        let customAttr = CustomAttribute (compilerGeneratedAttrCtor)
                        delmeth.CustomAttributes.Add (customAttr)

                        meth.Parameters
                        |> Seq.iter delmeth.Parameters.Add

                        del.Methods.Add (delmeth)

                        x.Module.Types.Add del
                    else
                        ()
                )
            )
            m.Write (this.AssemblyPath)
        )

        asmDef.Write this.AssemblyPath

        let asmBytes = System.IO.File.ReadAllBytes (this.AssemblyPath)
        let asm = Assembly.Load asmBytes

        let asmDef = AssemblyDefinition.ReadAssembly (this.AssemblyPath)  

        asmDef.Modules
        |> Seq.iter (fun m ->
            m.GetTypes ()
            |> Seq.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
            |> Seq.iter (fun x -> 
                x.Methods
                |> Array.ofSeq
                |> Array.iter (fun meth ->
                    if methodHasAttribute typeof<ExportAttribute> meth then
                        ()
                    else
                        meth.IsPInvokeImpl <- true
                        meth.IsPreserveSig <- true
                        meth.CallingConvention <- MethodCallingConvention.C
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