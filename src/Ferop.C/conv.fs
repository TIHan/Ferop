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
    Functions : MethodInfo list
    ExportedFunctions : MethodInfo list }

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

let makeCStructName (env: CEnv) (typ: Type) = sprintf "%s_%s" env.Name typ.Name

let makeCStruct = function 
    | CDecl.Struct (name, fields) -> 
        { Name = name; Fields = fields }
    | _ -> failwith "Invalid struct"

let tryLookupStruct env (typ: Type) = 
    env.Decls 
    |> List.tryFind (function | CDecl.Struct (x,_) -> x = makeCStructName env typ | _ -> false)
    |> function
    | None -> None
    | Some x -> Some <| makeCStruct x

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
    | x when x = typeof<nativeint> -> Some <| Pointer None
    | x when isTypeUnmanaged x ->
        match tryLookupStruct env x with
        | None -> None
        | Some x -> Some <| Struct x
    | _ -> None

and lookupCType env typ =
    match tryLookupCType env typ with
    | None -> failwithf "%A not supported." typ.FullName
    | Some x -> x

and makeCField typ name = { Type = typ; Name = name }

and makeCFields env (typ: Type) =
    properties typ
    |> List.map (fun x ->
        let ctype = lookupCType env x.PropertyType
        makeCField ctype x.Name)

let makeReturnType env = function
    | x when x = typeof<Void> -> None
    | x -> Some <| lookupCType env x

let makeParameter env (info: ParameterInfo) = (lookupCType env info.ParameterType, info.Name)

let makeParameters env infos = infos |> List.ofArray |> List.map (makeParameter env)

let makeParameterType env (info: ParameterInfo) = lookupCType env info.ParameterType

let makeParameterTypes env infos = infos |> List.ofArray |> List.map (makeParameterType env)

let rec makeCExpr = function
    | Call (_, _, exprList) -> makeCExpr exprList.[0]

    | Lambda (_, body) -> makeCExpr body

    | Value (value, _) -> Text <| value.ToString ()

    | x -> failwithf "Expression, %A, not supported." x

//-------------------------------------------------------------------------
// CDecls
//-------------------------------------------------------------------------

let makeCDeclFunction (env: CEnv) (func: MethodInfo) =
    let returnType = makeReturnType env func.ReturnType
    let name = sprintf "%s_%s" env.Name func.Name
    let parameters = func.GetParameters () |> makeParameters env
    let expr = methodExpr func |> makeCExpr

    CDecl.Function (returnType, name, parameters, expr)

let makeCDeclFunctionPointer (env: CEnv) (func: MethodInfo) =
    let returnType = makeReturnType env func.ReturnType
    let name = sprintf "fs_%s_%s" env.Name func.Name
    let parameterTypes = func.GetParameters () |> makeParameterTypes env

    CDecl.FunctionPointer (returnType, name, parameterTypes)

let rec makeCDeclStructFields env (typ: Type) =
    properties typ
    |> List.fold (fun (env, fields) x -> 
        match tryLookupCType env x.PropertyType with
        | None -> 
            let env = makeCDeclStruct env x.PropertyType
            env, makeCField (lookupCType env x.PropertyType) x.Name :: fields
        | Some ctype -> env, makeCField ctype x.Name :: fields) (env, [])
            
and makeCDeclStruct env (typ: Type) =
    match tryLookupCType env typ with
    | Some _ -> env
    | _ ->
        let name = makeCStructName env typ
        let env', fields = makeCDeclStructFields env typ

        { env' with Decls = CDecl.Struct (name, fields) :: env'.Decls }

let makeCDeclStructs (env: CEnv) modul =
    let env' =
        match modul.Functions with
        | [] -> failwith "There are no functions."
        | funcs ->
            funcs
            |> List.map (fun x -> x.GetParameters () |> List.ofArray)
            |> List.reduce (fun x y -> x @ y)
            |> List.map (fun x -> x.ParameterType)
            |> List.filter (fun x -> not x.IsPrimitive && isTypeUnmanaged x)
            |> List.fold (fun env x -> makeCDeclStruct env x) env
    { env' with 
        Decls = 
            match env'.Decls with
            | [] -> []
            | x ->  List.rev x }

let makeCDeclFunctions env modul =
    let funcs = modul.Functions |> List.map (makeCDeclFunction env)
    let funcPtrs = modul.ExportedFunctions |> List.map (makeCDeclFunctionPointer env)
    { env with Decls = env.Decls @ funcs @ funcPtrs }

//-------------------------------------------------------------------------
// CEnv
//-------------------------------------------------------------------------

let makeCEnv modul =
    let env = makeEmptyEnv modul.Name
    let env' = makeCDeclStructs env modul
    makeCDeclFunctions env' modul
