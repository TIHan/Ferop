module Ferop.CConversion

open System
open System.Reflection

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
 
open Ferop.CTypedAST

let private errorMsg = "This should not be called directly. Instead, call the generated version."

let C (code: string) =
    code |> ignore
    failwith errorMsg

let CExtern () = failwith errorMsg

let methodExpr meth =
    match Expr.TryGetReflectedDefinition meth with
    | None -> failwithf "Reflected definition for %s not found" meth.Name
    | Some expr -> expr

let makeCType = function
    | x when x = typeof<byte> ->    Byte
    | x when x = typeof<sbyte> ->   SByte
    | x when x = typeof<uint16> ->  UInt16
    | x when x = typeof<int16> ->   Int16
    | x when x = typeof<uint32> ->  UInt32
    | x when x = typeof<int32> ->   Int32
    | x when x = typeof<uint64> ->  UInt64
    | x when x = typeof<int64> ->   Int64
    | x when x = typeof<single> ->  Float
    | x when x = typeof<double> ->  Double
    | x -> failwithf "%A not supported." x.FullName

let makeReturnType = function
    | x when x = typeof<Void> -> None
    | x -> Some <| makeCType x

let makeParameter (info: ParameterInfo) = CLocalVar (makeCType info.ParameterType, info.Name)

let makeParameters (infos: ParameterInfo []) = infos |> List.ofArray |> List.map makeParameter

let rec makeCExpr = function
    | SpecificCall <@ CExtern @> (_, _, _) -> Text ""

    | Call (_, _, exprList) -> makeCExpr exprList.[0]

    | Lambda (_, body) -> makeCExpr body

    | Value (value, _) -> Text <| value.ToString ()

    | x -> failwithf "Expression, %A, not supported." x

let makeCFunction (func: MethodInfo) =
    let returnType = makeReturnType func.ReturnType
    let name = func.Name
    let parameters = func.GetParameters () |> makeParameters
    let expr = methodExpr func |> makeCExpr

    Function (returnType, name, parameters, expr)
