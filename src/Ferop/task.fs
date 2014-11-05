namespace Ferop

open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities

type public WeavingTask () =
    inherit Task ()

    [<Required>]
    member val AssemblyPath : string = "" with get, set

    member val IntermediateDir : string = "" with get, set
    member val KeyFilePath : string = "" with get, set
    member val SignAssembly : bool = false with get, set

    [<Required>]
    member val ProjectDirectory = "" with get, set

    member val References = "" with get, set
    member val ReferenceCopyLocalPaths = Array.empty<ITaskItem> with get, set

    [<Required>]
    member val SolutionDir = "" with get, set

    member val DefineConstants = "" with get, set

    override this.Execute () : bool =  
        let asm = Assembly.LoadFile (this.AssemblyPath)    
        FSharp.Interop.FeropCompiler.C.compile this.AssemblyPath this.ProjectDirectory this.ProjectDirectory true FSharp.Interop.Ferop.Platform.Auto asm |> ignore
        true