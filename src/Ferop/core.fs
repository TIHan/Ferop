module internal Ferop.Core

open System
open System.Reflection

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Ferop.Helpers
open Ferop.Code

type Parameter = Parameter of string * System.Type

type FunctionInline =
    {
    Name: string
    ReturnType: Type
    Parameters: Parameter list
    Code: string }

type FunctionExtern =
    {
    Name: string
    ReturnType: Type
    Parameters: Parameter list }

type Function =
    | Inline of FunctionInline
    | Extern of FunctionExtern

type Module =
    {
    Name: string
    Functions: Function list
    Attributes: CustomAttributeData list }

let methodExpr meth =
    match Expr.TryGetReflectedDefinition meth with
    | None -> failwithf "Reflected definition for %s not found" meth.Name
    | Some expr -> expr

let makeParameter (param: ParameterInfo) =
    Parameter (param.Name, param.ParameterType)

let findParameters (meth: MethodInfo) =
    meth.GetParameters ()
    |> List.ofArray
    |> List.map makeParameter

let makeFunction (meth: MethodInfo) =
    let name = meth.Name
    let returnType = meth.ReturnType
    let parameters = findParameters meth
    let expr = methodExpr meth

    let rec make = function
        | SpecificCall <@ C @> (_, _, exprList) ->
            match exprList.[0] with
            | Value (value, _) ->
                Inline { Name = name; ReturnType = returnType; Parameters = parameters; Code = value.ToString () }
            | _ -> failwith "Invalid interop."

        | SpecificCall <@ CExtern @> (_, _, _) ->
            Extern { Name = name; ReturnType = returnType; Parameters = parameters }

        | Call (_, _, exprList) ->
            make exprList.[0]

        | Lambda (_, body) ->
            make body

        | x -> failwith "Invalid interop."

    make expr

let findFunctions (typ: Type) =
    Type.moduleFunctions typ
    |> List.map makeFunction

let findAttributes (typ: Type) =
    typ.CustomAttributes
    |> List.ofSeq

let makeModule (typ: Type) =
    let name = typ.Name
    let funcs = findFunctions typ
    let attrs = findAttributes typ 

    { Name = name; Functions = funcs; Attributes = attrs }