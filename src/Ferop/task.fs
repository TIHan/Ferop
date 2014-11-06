namespace Ferop

open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open Mono.Cecil

type public WeavingTask () =
    inherit Task ()

    [<Required>]
    member val AssemblyPath : string = "" with get, set

    [<Required>]
    member val ProjectDirectory : string = "" with get, set

    override this.Execute () : bool =  
        //failwith this.AssemblyPath
        let asm = AssemblyDefinition.ReadAssembly (this.AssemblyPath)  
        //FSharp.Interop.FeropCompiler.C.compile this.AssemblyPath this.ProjectDirectory this.ProjectDirectory true FSharp.Interop.Ferop.Platform.Auto asm |> ignore
        true