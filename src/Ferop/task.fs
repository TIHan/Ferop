namespace Ferop

open System
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open Mono.Cecil

open FSharp.Interop.Ferop

type public WeavingTask () =
    inherit Task ()

    let hasAttribute (typ: Type) (typDef: TypeDefinition) =
        typDef.CustomAttributes
        |> Seq.exists (fun x ->
            x.AttributeType.FullName.Contains(typ.Name))

    [<Required>]
    member val AssemblyPath : string = "" with get, set

    [<Required>]
    member val ProjectDirectory : string = "" with get, set

    override this.Execute () : bool =  
        let asm = AssemblyDefinition.ReadAssembly (this.AssemblyPath)  

        asm.Modules
        |> Seq.iter (fun m ->
            m.GetTypes ()
            |> Seq.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
            |> Seq.iter (fun x -> ()))
        //FSharp.Interop.FeropCompiler.C.compile this.AssemblyPath this.ProjectDirectory this.ProjectDirectory true FSharp.Interop.Ferop.Platform.Auto asm |> ignore
        true