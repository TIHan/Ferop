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

let runtimeFields (typ: Type) = typ.GetRuntimeFields () |> List.ofSeq

let allNestedRuntimeFieldTypes (typ: Type) =
    let f (x: Type) : Type list =
        (x.GetRuntimeFields ())
        |> Seq.map (fun x -> x.FieldType)
        |> Seq.distinctBy (fun x -> x.FullName)
        |> List.ofSeq

    let rec fr (types: Type list) = function
        | [] -> types
        | x :: xs as nested ->
            let typesToCover = types @ nested
            let refs =
                (f x)
                |> List.filter (fun x -> not (typesToCover |> List.exists (fun y -> y.FullName = x.FullName)))
            fr (x :: types) (xs @ refs)
                
    fr [] (f typ)  

let isTypeUnmanaged (typ: Type) =
    let check (x: Type) = (x.IsValueType && not x.IsGenericType)
    match check typ with
    | false -> false
    | _ -> allNestedRuntimeFieldTypes typ |> List.forall check

let methodExpr meth =
    match Expr.TryGetReflectedDefinition meth with
    | None -> failwithf "Reflected definition for %s not found" meth.Name
    | Some expr -> expr

let lookupStruct env (typ: Type) = env.StructMap |> Map.tryFind (typ.Name)

let rec makeCType env = function
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
    | x when isTypeUnmanaged x ->
        match lookupStruct env x with
        | None -> failwithf "%A not found." x.Name
        | Some x -> Struct x
    | x -> failwithf "%A not supported." x.FullName

and makeCStruct env (typ: Type) =
    let name = typ.Name
    let fields =
        runtimeFields typ
        |> List.map (fun x -> CField (makeCType env x.FieldType, x.Name))

    Struct { Name = name; Fields = fields }

let makeReturnType env = function
    | x when x = typeof<Void> -> None
    | x -> Some <| makeCType env x

let makeParameter env (info: ParameterInfo) = CLocalVar (makeCType env info.ParameterType, info.Name)

let makeParameters env (infos: ParameterInfo []) = infos |> List.ofArray |> List.map (makeParameter env)

let rec makeCExpr = function
    | SpecificCall <@ CExtern @> (_, _, _) -> Text ""

    | Call (_, _, exprList) -> makeCExpr exprList.[0]

    | Lambda (_, body) -> makeCExpr body

    | Value (value, _) -> Text <| value.ToString ()

    | x -> failwithf "Expression, %A, not supported." x

let makeCFunction env (func: MethodInfo) =
    let returnType = makeReturnType env func.ReturnType
    let name = func.Name
    let parameters = func.GetParameters () |> makeParameters env
    let expr = methodExpr func |> makeCExpr

    Function (returnType, name, parameters, expr)

let makeCDecl env func = makeCFunction env func

let addCDecls env decls = { env with Decls = env.Decls @ decls }

let makeTypedAst funcs =
    let env = makeEmptyEnv ()
    let decls = funcs |> List.map (makeCDecl env)
    addCDecls env decls
