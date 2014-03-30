module internal Ferop.Helpers

open System
open System.Reflection

open Microsoft.FSharp.Reflection

[<RequireQualifiedAccess>]
module Type =
    let tryMethod name (t: Type) =
        match t.GetMethod name with
        | null  -> None
        | x     -> Some x
 
    let recordFields (t: Type) = FSharpType.GetRecordFields t |> List.ofArray
 
    let methods (t: Type) = t.GetMethods () |> List.ofArray
 
    let moduleFunctions (t: Type) =
        methods t
        |> List.filter (fun x -> 
        x.Name <> "GetType" && 
        x.Name <> "GetHashCode" && 
        x.Name <> "Equals" && 
        x.Name <> "ToString")

[<RequireQualifiedAccess>]
module Assembly =
    let types (asm: Assembly) =
        asm.GetTypes ()
        |> List.ofArray

    let modules (asm: Assembly) =
        asm
        |> types
        |> List.filter FSharpType.IsModule
