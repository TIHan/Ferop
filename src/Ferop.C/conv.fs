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

type FsModule = { 
    Name : string
    Functions : MethodInfo list }

let private errorMsg = "This should not be called directly. Instead, call the generated version."

let C (code: string) =
    code |> ignore
    failwith errorMsg

let CExtern () = failwith errorMsg

let runtimeFields (typ: Type) = typ.GetRuntimeFields () |> List.ofSeq

let properties (typ: Type) = typ.GetProperties () |> List.ofSeq

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

let rec isTypeUnmanaged (typ: Type) =
    let check (x: Type) = 
        (x.IsValueType && not x.IsGenericType) ||
        (x.IsPointer && (x.GenericTypeArguments |> Array.forall (isTypeUnmanaged)))
    match check typ with
    | false -> false
    | _ -> allNestedRuntimeFieldTypes typ |> List.forall check

let methodExpr meth =
    match Expr.TryGetReflectedDefinition meth with
    | None -> failwithf "Reflected definition for %s not found" meth.Name
    | Some expr -> expr

let tryLookupStruct env (typ: Type) = env.Structs |> List.tryFind (fun (CStruct(x,_)) -> x = typ.Name)

let rec tryLookupCType env = function
    | x when x = typeof<byte> ->    Some Byte
    | x when x = typeof<sbyte> ->   Some SByte
    | x when x = typeof<uint16> ->  Some UInt16
    | x when x = typeof<int16> ->   Some Int16
    | x when x = typeof<uint32> ->  Some UInt32
    | x when x = typeof<int32> ->   Some Int32
    | x when x = typeof<uint64> ->  Some UInt64
    | x when x = typeof<int64> ->   Some Int64
    | x when x = typeof<single> ->  Some Float
    | x when x = typeof<double> ->  Some Double
    | x when x = typeof<bool> ->    Some Int32
    | x when x = typeof<nativeint> -> Some <| Pointer (Void)
    | x when isTypeUnmanaged x ->
        match tryLookupStruct env x with
        | None -> None
        | Some x -> Some <| Struct x
    | _ -> None

and lookupCType env typ =
    match tryLookupCType env typ with
    | None -> failwithf "%A not supported." typ.FullName
    | Some x -> x

and makeCStruct env (typ: Type) =
    let name = typ.Name
    let env, fields =
        properties typ
        |> List.fold (fun (env, fields) x -> 
            match tryLookupCType env x.PropertyType with
            | Some ctype ->
                env, CField (ctype, x.Name) :: fields
            | None ->
                let env = makeCStruct env x.PropertyType
                env,CField (lookupCType env x.PropertyType, x.Name) :: fields) (env, [])

    { env with Structs = CStruct (name, List.rev fields) :: env.Structs }

let makeReturnType env = function
    | x when x = typeof<Void> -> None
    | x -> Some <| lookupCType env x

let makeParameter env (info: ParameterInfo) = CVar (lookupCType env info.ParameterType, info.Name)

let makeParameters env infos = infos |> List.ofArray |> List.map (makeParameter env)

let rec makeCExpr = function
    | SpecificCall <@ CExtern @> (_, _, _) -> Text ""

    | Call (_, _, exprList) -> makeCExpr exprList.[0]

    | Lambda (_, body) -> makeCExpr body

    | Value (value, _) -> Text <| value.ToString ()

    | x -> failwithf "Expression, %A, not supported." x

let makeCFunction env (func: MethodInfo) =
    let returnType = makeReturnType env func.ReturnType
    let name = sprintf "%s_%s" env.Name func.Name
    let parameters = func.GetParameters () |> makeParameters env
    let expr = methodExpr func |> makeCExpr

    Function (returnType, name, parameters, expr)

let makeCDecl env func = makeCFunction env func

let makeCStructs env modul =
    modul.Functions
    |> List.map (fun x -> x.GetParameters () |> List.ofArray)
    |> List.reduce (fun x y -> x @ y)
    |> List.map (fun x -> x.ParameterType)
    |> Seq.ofList
    |> Seq.distinct
    |> List.ofSeq
    |> List.filter (fun x -> not x.IsPrimitive && isTypeUnmanaged x)
    |> List.fold (fun env x -> makeCStruct env x) env

let makeCDecls env modul =
    { env with Decls = modul.Functions |> List.map (makeCDecl env) }

let makeCEnv modul =
    let env = makeEmptyEnv modul.Name
    let env' = makeCStructs env modul
    makeCDecls env' modul
