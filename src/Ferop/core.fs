module internal Ferop.Core

open System
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Ferop.Helpers
open Ferop.Code

open FSharp.Control.IO

type Parameter = {
    Name: string
    Type: Type }

type FunctionInline = {
    Name: string
    ReturnType: Type
    Parameters: Parameter list
    Code: string }

type FunctionExtern = {
    Name: string
    ReturnType: Type
    Parameters: Parameter list }

type Function =
    | Inline of FunctionInline
    | Extern of FunctionExtern

type Module = {
    Name: string
    Functions: Function list
    Attributes: CustomAttributeData list } with

    member this.IncludeAttributes =
        this.Attributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<IncludeAttribute>)

    member this.ClangFlagsOsxAttribute =
        this.Attributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<ClangFlagsOsxAttribute>)
        |> Seq.exactlyOne

    member this.ClangLibsOsxAttribute =
        this.Attributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<ClangLibsOsxAttribute>)
        |> Seq.exactlyOne

    member this.Includes =
        this.IncludeAttributes
        |> Seq.map (fun x -> Seq.exactlyOne x.ConstructorArguments)
        |> Seq.map (fun x -> "#include " + (x.Value :?> string))
        |> Seq.reduce (fun x y -> x + "\n" + y)

    member this.ClangFlagsOsx =
        let attr = this.ClangFlagsOsxAttribute
        let args = Seq.exactlyOne attr.ConstructorArguments
        args.Value :?> string

    member this.ClangLibsOsx =
        let attr = this.ClangLibsOsxAttribute
        let args = Seq.exactlyOne attr.ConstructorArguments
        args.Value :?> string

let methodExpr meth =
    match Expr.TryGetReflectedDefinition meth with
    | None -> failwithf "Reflected definition for %s not found" meth.Name
    | Some expr -> expr

let makeParameter (param: ParameterInfo) =
    { Name = param.Name; Type = param.ParameterType }

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
    let name = typ.FullName
    let funcs = findFunctions typ
    let attrs = findAttributes typ 

    { Name = name; Functions = funcs; Attributes = attrs }

let definePInvokeMethod (tb: TypeBuilder) dllName name entryName returnType parameters = io {
    let meth = 
        tb.DefinePInvokeMethod (
            name,
            dllName,
            entryName,
            MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PinvokeImpl,
            CallingConventions.Standard,
            returnType,
            parameters |> List.map (fun x -> x.Type) |> Array.ofList,
            CallingConvention.Cdecl,
            CharSet.Ansi)

    meth.SetImplementationFlags (meth.GetMethodImplementationFlags () ||| MethodImplAttributes.PreserveSig) }