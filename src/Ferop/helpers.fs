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
       
    let allNestedTypes (typ: Type) =
        let f (x: Type) : Type list =
            (x.GetRuntimeFields ())
            |> Seq.map (fun x -> x.FieldType)
            |> Seq.distinctBy (fun x -> x.FullName)
            |> List.ofSeq

        let rec allNestedTypes (types: Type list) = function
            | [] -> types
            | x :: xs as nested ->
                let typesToCover = types @ nested
                let refs =
                    (f x)
                    |> List.filter (fun x -> not (typesToCover |> List.exists (fun y -> y.FullName = x.FullName)))
                allNestedTypes (x :: types) (xs @ refs)
                    
        allNestedTypes [] (f typ)           

    let isUnmanaged (typ: Type) =
        let check (x: Type) = x.IsValueType && not x.IsGenericType
        match check typ with
        | false -> false
        | _ -> allNestedTypes typ |> List.forall check

[<RequireQualifiedAccess>]
module Assembly =
    let types (asm: Assembly) =
        asm.GetTypes ()
        |> List.ofArray

    let modules (asm: Assembly) =
        asm
        |> types
        |> List.filter FSharpType.IsModule
