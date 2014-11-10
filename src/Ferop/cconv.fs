module internal FSharp.Interop.FeropInternal.CConversion

open System
open System.Reflection
open System.Runtime.CompilerServices

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
 
open CTypedAST

type CConvInfo = { 
    Name: string
    Functions: MethodInfo list
    ExportedFunctions: MethodInfo list
    IsCpp: bool }

let runtimeFields (typ: Type) = 
    typ.GetRuntimeFields () 
    |> Seq.filter (fun x -> not x.IsStatic)
    |> List.ofSeq
    |> List.rev

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

let rec isTypeBlittable (typ: Type) =
    let check (x: Type) = 
        (x.IsValueType && not x.IsGenericType) ||
        (x.IsPointer && (x.GenericTypeArguments |> Array.forall (isTypeBlittable)))
    match check typ with
    | false -> false
    | _ -> allNestedRuntimeFieldTypes typ |> List.forall check

let typeHasAttributeType (attrType: Type) (typ: Type) =
    typ.GetCustomAttributesData ()
    |> Seq.map (fun x -> x.AttributeType)
    |> Seq.exists ((=)attrType)

let tryMakeCExpr (meth: MethodInfo) =
    try
        let body = meth.GetMethodBody ()
        let ilBytes = body.GetILAsByteArray ()

        let textExpr = meth.Module.ResolveString (BitConverter.ToInt32 (ilBytes, 2))

        Some (CExpr.Text textExpr)
    with | _ -> None

let makeCTypeName (env: CEnv) (typ: Type) = sprintf "%s_%s" env.Name typ.Name

let makeCFunctionName (env: CEnv) (func: MethodInfo) = sprintf "%s_%s" env.Name func.Name

let makeCStruct = function
    | CDecl.Struct struc -> 
        { CStruct.Name = struc.Name; Fields = struc.Fields }
    | _ -> failwith "Invalid struct"

let makeCEnum = function
    | CDecl.Enum enum ->
        { CEnum.Name = enum.Name }
    | _ -> failwith "Invalid enum"

let makeCFunction = function
    | CDecl.FunctionPointer func ->
        { CFunction.ReturnType = func.ReturnType; Name = func.Name; ParameterTypes = func.ParameterTypes }
    | _ -> failwith "Invalid function"

let tryLookupStruct env (typ: Type) = 
    env.Decls 
    |> List.tryFind (function | CDecl.Struct x -> x.Name = makeCTypeName env typ | _ -> false)
    |> function
    | None -> None
    | Some x -> Some <| makeCStruct x

let tryLookupEnum env (typ: Type) =
    env.Decls
    |> List.tryFind (function | CDecl.Enum x -> x.Name = makeCTypeName env typ | _ -> false)
    |> function
    | None -> None
    | Some x -> Some <| makeCEnum x

let tryLookupFunction env (typ: Type) =
    env.Decls
    |> List.tryFind (function | CDecl.FunctionPointer x -> x.Name = makeCTypeName env typ | _ -> false)
    |> function
    | None -> None
    | Some x -> Some <| makeCFunction x

let isTypePointer (typ: Type) = 
    typ.IsArray || typ.IsPointer || typ.IsByRef ||
    typ = typeof<nativeint> || typ = typeof<unativeint>

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
    | x when x = typeof<unativeint> -> Some <| Pointer None
    | x when x = typeof<nativeint> ->  Some <| Pointer None
    | x when isTypePointer x ->
        let elType = x.GetElementType ()

        if elType = null || isTypePointer elType
        then None
        else

        match tryLookupCType env elType with
        | None -> None
        | Some x -> Some <| Pointer (Some x)
    | x when x.IsEnum ->
        match tryLookupEnum env x with
        | None -> None
        | Some x -> Some <| CType.Enum x
    | x when isTypeBlittable x ->
        match tryLookupStruct env x with
        | None -> None
        | Some x -> Some <| CType.Struct x
    | x when x.BaseType = typeof<MulticastDelegate> ->
        x.GetMethod "Invoke" |> ignore // make sure there is a method named "Invoke"

        match 
            x.CustomAttributes
            |> Seq.tryFind (fun attr -> attr.AttributeType = typeof<System.Runtime.InteropServices.UnmanagedFunctionPointerAttribute>) with
        | None -> failwithf "Delegate, %s, must have the attribute, UnmanagedFunctionPointer, from System.Runtime.InteropServices with CallingConvention.Cdecl." x.Name
        | Some attr ->
            let attr = Seq.exactlyOne attr.ConstructorArguments
            let attr = attr.Value :?> System.Runtime.InteropServices.CallingConvention
            if attr <> System.Runtime.InteropServices.CallingConvention.Cdecl
            then failwithf "Delegate, %s, must have CallingConvention.Cdecl." x.Name
            else ()

        match tryLookupFunction env x with
        | None -> None
        | Some x -> Some <| CType.Function x
    | _ -> None

let lookupCType env typ =
    match tryLookupCType env typ with
    | None -> failwithf "%A is not supported." typ.FullName
    | Some x -> x

let makeCField typ name = { CField.Type = typ; Name = name }

let makeReturnType env = function
    | x when x = typeof<Void> -> None
    | x when x.IsArray ->  failwithf "Arrays cannot be return types."
    | x when x.IsPointer -> failwithf "Pointers cannot be return types."
    | x when x.IsByRef -> failwithf "ByRefs cannot be return types."
    | x -> Some <| lookupCType env x

let makeParameter env (info: ParameterInfo) = 
    if info.Name = null then
        failwithf "No parameter name at position %i in method %s" info.Position info.Member.Name
    { CParameter.Type = lookupCType env info.ParameterType; Name = info.Name }

let makeParameters env infos = infos |> List.ofArray |> List.map (makeParameter env)

let makeParameterType env (info: ParameterInfo) = lookupCType env info.ParameterType

let makeParameterTypes env infos = infos |> List.ofArray |> List.map (makeParameterType env)

let makeCExprFallback (env: CEnv) (func: MethodInfo) =
    match func.GetParameters () with
    | [|x|] when x.ParameterType.BaseType = typeof<MulticastDelegate> && func.Name.Contains("_ferop_set_") ->
        let typ = x.ParameterType
        let name = sprintf "%s_%s" env.Name (typ.Name.Replace ("Delegate", ""))
        Text <| sprintf "%s = ptr;" name
    | _ -> failwithf "Function, %A, not supported." func.Name

//-------------------------------------------------------------------------
// CDecls
//-------------------------------------------------------------------------

let makeCDeclFunction env (meth: MethodInfo) =
    let returnType = makeReturnType env meth.ReturnType
    let name = makeCFunctionName env meth
    let parameters = meth.GetParameters () |> makeParameters env
    let expr =
        match tryMakeCExpr meth with
        | None -> makeCExprFallback env meth
        | Some x -> x

    { ReturnType = returnType; Name = name; Parameters = parameters; Expr = expr }

let makeCDeclFunctionPrototype env (meth: MethodInfo) : CDeclFunctionPrototype =
    let decl = makeCDeclFunction env meth
    let returnType = decl.ReturnType
    let name = decl.Name
    let parameterTypes =
        decl.Parameters
        |> List.map (fun x -> x.Type)

    { ReturnType = returnType; Name = name; ParameterTypes = parameterTypes }

let makeCDeclFunctionPointer (env: CEnv) (typ: Type) =
    let func = typ.GetMethod "Invoke"
    let returnType = makeReturnType env func.ReturnType
    let name = makeCTypeName env typ
    let parameterTypes = func.GetParameters () |> makeParameterTypes env

    { ReturnType = returnType; Name = name; ParameterTypes = parameterTypes }

let rec makeCDeclStructFields env (typ: Type) =
    runtimeFields typ
    |> List.fold (fun (env, fields) x ->
        let name = x.Name.Replace ("@", "") 

        match tryLookupCType env x.FieldType with
        | None -> 
            let env = makeCDeclStruct env x.FieldType
            env, makeCField (lookupCType env x.FieldType) name :: fields
        | Some ctype -> env, makeCField ctype name :: fields) (env, [])
            
and makeCDeclStruct env (typ: Type) =
    match tryLookupCType env typ with
    | Some _ -> env
    | _ ->
        let name = makeCTypeName env typ
        let env', fields = makeCDeclStructFields env typ

        if typ.IsAutoLayout || typ.IsExplicitLayout then
            failwithf "Struct, %s, is not allowed to use auto or explicit layout." typ.Name

        { env' with Decls = CDecl.Struct ({ CDeclStruct.Name = name; Fields = fields }) :: env'.Decls }

let makeCDeclEnum env (typ: Type) =
    match typ.GetEnumUnderlyingType () <> typeof<int> with
    | true -> failwithf "Enum, %s, does not have an underlying type of int." typ.Name
    | _ ->

    let name = makeCTypeName env typ
    let constNames = Enum.GetNames typ |> Array.map (fun x -> name + "_" + x)
    let fields = typ.GetFields () |> Array.filter (fun x -> not <| x.Name.Equals("value__"))
    let intValues = Array.zeroCreate<int> constNames.Length

    for i = 0 to intValues.Length - 1 do
        intValues.[i] <- fields.[i].GetRawConstantValue() :?> int

    let consts = 
        Array.map2 (fun (x: string) (y: int) -> { CEnumConst.Name = x; Value = y }) 
            constNames intValues
        |> List.ofArray

    { CDeclEnum.Name = name; Consts = consts }

let makeCDeclVar env name typ =
    let ctype = lookupCType env typ

    { CDeclVar.Name = name; Type = ctype }

let makeCDeclExtern env name typ =
    let ctype = lookupCType env typ

    { CDeclExtern.Type = ctype; Name = name }

let makeCDeclStructs (env: CEnv) = function
    | [] -> env
    | funcs ->
        let env' = funcs |> List.fold (fun env x -> makeCDeclStruct env x) env
        { env' with 
            Decls = 
                match env'.Decls with
                | [] -> []
                | x ->  List.rev x }

let makeCDeclEnums (env: CEnv) = function
    | [] -> env
    | enums ->
        let decls = enums |> List.map (fun x -> makeCDeclEnum env x) |> List.map CDecl.Enum
        { env with Decls = env.Decls @ decls }

let makeCDeclFunctions (env: CEnv) = function
    | [] -> env
    | funcs ->
        let decls = funcs |> List.map (makeCDeclFunction env) |> List.map CDecl.Function
        { env with Decls = env.Decls @ decls }

let makeCDeclFunctionPrototypes (env: CEnv) = function
    | [] -> env
    | funcs ->
        let decls = funcs |> List.map (makeCDeclFunctionPrototype env) |> List.map CDecl.FunctionPrototype
        { env with Decls = env.Decls @ decls }

let makeCDeclFunctionPointers (env: CEnv) = function
    | [] -> env
    | funcPtrs ->
        let decls = funcPtrs |> List.map (makeCDeclFunctionPointer env) |> List.map CDecl.FunctionPointer
        { env with Decls = env.Decls @ decls }

let makeCDeclGlobalVars (env: CEnv) = function
    | [] -> env
    | types ->
        let decls = 
            types
            |> List.map (fun (x: Type) ->
                let name = sprintf "%s_%s" env.Name (x.Name.Replace ("Delegate", ""))
                makeCDeclVar env name x) 
            |> List.map GlobalVar
        { env with Decls = env.Decls @ decls }
        
let makeCDeclExterns (env: CEnv) = function
    | [] -> env
    | types ->
        let decls = 
            types
            |> List.map (fun (x: Type) -> 
                let name = sprintf "%s_%s" env.Name (x.Name.Replace ("Delegate", ""))
                makeCDeclExtern env name x) 
            |> List.map CDecl.Extern
        { env with Decls = env.Decls @ decls }

let makeCDecls (env: CEnv) info =
    let funcs = info.Functions
    let exportedFuncs = info.ExportedFunctions
    let dels =
        funcs @ exportedFuncs
        |> List.map (fun x -> x.GetParameters () |> List.ofArray)
        |> List.reduce (fun x y -> x @ y)
        |> List.map (fun x -> x.ParameterType)
        |> List.filter (fun x -> x.BaseType = typeof<MulticastDelegate>)

    // If the delegate is compiler generated, we are creating
    // an extern function.
    let instanceDels =
        dels
        |> List.filter (typeHasAttributeType typeof<CompilerGeneratedAttribute>)

    let structs =
        funcs @ exportedFuncs
        |> List.map (fun x -> (x.GetParameters () |> Array.map (fun x -> x.ParameterType) |> List.ofArray) @ [x.ReturnType])
        |> List.reduce (fun x y -> x @ y)
        |> List.map (fun x -> if isTypePointer x && x.GetElementType () <> null then x.GetElementType () else x)
        |> List.filter (fun x -> not (x = typeof<Void>) && not x.IsPrimitive && not x.IsEnum && isTypeBlittable x)

    let enums =
        funcs @ exportedFuncs
        |> List.map (fun x -> x.GetParameters () |> List.ofArray)
        |> List.reduce (fun x y -> x @ y)
        |> List.map (fun x -> x.ParameterType)
        |> List.filter (fun x -> x.IsEnum)

    let env' = makeCDeclStructs env structs
    let env'' = makeCDeclEnums env' enums
    let env''' = makeCDeclFunctionPointers env'' dels
    let env'''' = makeCDeclFunctionPrototypes env''' funcs
    let env''''' = makeCDeclGlobalVars env'''' instanceDels
    let env'''''' = makeCDeclFunctions env''''' funcs
    let env''''''' = makeCDeclExterns env'''''' instanceDels
    env'''''''

//-------------------------------------------------------------------------
// CEnv
//-------------------------------------------------------------------------

let makeCEnv info =
    let env = makeEmptyEnv info.Name
    makeCDecls { env with IsCpp = info.IsCpp } info
