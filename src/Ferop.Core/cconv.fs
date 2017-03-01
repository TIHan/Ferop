module internal Ferop.CConversion

open System
open System.Reflection
open System.Reflection.Emit
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

#nowarn "9"
 
open CTypedAST

type CConvInfo = 
    { 
        Name: string
        ImportedFunctions: MethodInfo list
        ExportedFunctions: MethodInfo list 
        IsCpp: bool
    }

let byteIsOpCode (opCode: OpCode) (x: byte)  = x = byte opCode.Value

/// Find properties that ONLY return the backing field.
let propertiesWithFields (typ: Type) =
    let fields = typ.GetRuntimeFields ()

    typ.GetProperties ()
    |> Seq.filter (fun x -> not x.GetMethod.IsStatic)
    |> Seq.choose (fun prop ->
        let meth = prop.GetMethod
        let body = meth.GetMethodBody ()
        let ilBytes = body.GetILAsByteArray ()

        match ilBytes |> Array.tryFindIndex (byteIsOpCode OpCodes.Ldfld) with
        | None -> None
        | Some ldfld_index ->

        let field = meth.Module.ResolveField (BitConverter.ToInt32 (ilBytes.[ldfld_index+1..ldfld_index+4], 0))

        match fields |> Seq.tryFind (fun x -> x = field) with
        | None -> None
        | Some field ->

        let b = ilBytes.[ldfld_index+5]

        match byteIsOpCode OpCodes.Stloc_0 b with
        | true ->
            match ilBytes |> Array.tryFindIndex (byteIsOpCode OpCodes.Ret) with
            | None -> None
            | Some ret_index ->
                match byteIsOpCode OpCodes.Ldloc_0 ilBytes.[ilBytes.Length - 2] with
                | false -> None
                | _ -> Some (prop, field)
        | _ ->

        match byteIsOpCode OpCodes.Ret b with
        | true -> Some (prop, field)
        | _ -> None)
    |> Seq.distinctBy (fun (_,x) -> x)
    |> List.ofSeq

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
    |> Seq.map (fun x -> x.AttributeType.FullName)
    |> Seq.exists ((=)attrType.FullName)

let methodHasAttributeType (attrType: Type) (meth: MethodInfo) =
    meth.GetCustomAttributesData ()
    |> Seq.map (fun x -> x.AttributeType.FullName)
    |> Seq.exists ((=)attrType.FullName)

let makeCExpr (meth: MethodInfo) =
    try
        let body = meth.GetMethodBody ()
        let ilBytes = body.GetILAsByteArray ()

        let resolve i = meth.Module.ResolveString (BitConverter.ToInt32 (ilBytes, i))

        let textExpr = 
            try resolve 1
            with | _ -> resolve 2

        CExpr.Text textExpr
    with | _ ->
        failwithf "Error finding C/C++ code in function, %s" meth.Name

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

let makeCExprFallback (env: CEnv) (meth: MethodInfo) =
    match meth.GetParameters () with
    | [|x|] when x.ParameterType.BaseType = typeof<MulticastDelegate> && meth.Name.Contains("__ferop_set_exported__") ->
        let typ = x.ParameterType
        let name = sprintf "%s_%s" env.Name (typ.Name.Replace ("__ferop_exported__", ""))
        Text <| sprintf "%s = ptr;" name
    | _ -> failwithf "Function, %A, not supported." meth.Name

//-------------------------------------------------------------------------
// CDecls
//-------------------------------------------------------------------------

let makeCDeclFunction env (meth: MethodInfo) : CEnv * (CDeclFunction option) =
    let returnType = makeReturnType env meth.ReturnType
    let name = makeCFunctionName env meth
    let parameters = meth.GetParameters () |> makeParameters env
    let expr =
        if meth.Name.Contains("__ferop_set_exported__")
        then makeCExprFallback env meth
        else makeCExpr meth

    let func = { ReturnType = returnType; Name = name; Parameters = parameters; Expr = expr }
    { env with Decls = CDecl.Function func :: env.Decls }, Some func

let makeCDeclFunctionPrototype env (meth: MethodInfo) : CEnv * (CDeclFunctionPrototype option) =
    match makeCDeclFunction env meth with
    | _, Some decl ->
        let returnType = decl.ReturnType
        let name = decl.Name
        let parameterTypes =
            decl.Parameters
            |> List.map (fun x -> x.Type)

        let funcProto : CDeclFunctionPrototype = { ReturnType = returnType; Name = name; ParameterTypes = parameterTypes }
        { env with Decls = CDecl.FunctionPrototype funcProto :: env.Decls }, Some funcProto
    | _ -> env, None

let makeCDeclFunctionPointer (env: CEnv) (typ: Type) : CEnv * (CDeclFunctionPointer option) =
    let func = typ.GetMethod "Invoke"
    let returnType = makeReturnType env func.ReturnType
    let name = makeCTypeName env typ
    let parameterTypes = func.GetParameters () |> makeParameterTypes env

    let funcPointer = { ReturnType = returnType; Name = name; ParameterTypes = parameterTypes }
    { env with Decls = CDecl.FunctionPointer funcPointer :: env.Decls }, Some funcPointer

let filterName x =
    match x with
    | x when x >= 'a' && x <= 'z' -> true
    | x when x >= 'A' && x <= 'Z' -> true
    | x when x >= '0' && x <= '9' -> true
    | x when x = '_' -> true
    | _ -> false

let rec makeCFields env (typ: Type) : CEnv * CField list =
    let pfs = propertiesWithFields typ

    runtimeFields typ
    |> List.fold (fun (env, fields) x ->
        let name =
            // The name is determined by a property's getter only returning a backing field.
            // If so, the property's name is used; otherwise, use the field's name.
            match pfs |> List.tryFind (fun (_,fld) -> fld = x) with
            | None -> x.Name
            | Some (x,_) -> x.Name

        let chars = 
            name
            |> Seq.filter (fun x -> filterName (char x))
            |> Array.ofSeq

        let name = String (chars)

        match tryLookupCType env x.FieldType with
        | None -> 
            let env =
                if x.FieldType.IsEnum then
                    let env, _ = makeCDeclEnum env x.FieldType
                    env
                elif x.FieldType.IsValueType then 
                    let env, _ = makeCDeclStruct env x.FieldType
                    env
                else
                    failwithf "Improper type, %s." x.FieldType.Name

            env, makeCField (lookupCType env x.FieldType) name :: fields
        | Some ctype -> env, makeCField ctype name :: fields) (env, [])
            
and makeCDeclStruct env (typ: Type) : CEnv * (CDeclStruct option) =
    match tryLookupCType env typ with
    | Some _ -> env, None
    | _ ->
        let name = makeCTypeName env typ
        let env', fields = makeCFields env typ

        if typ.IsAutoLayout || typ.IsExplicitLayout then
            failwithf "Struct, %s, is not allowed to use auto or explicit layout." typ.Name

        let struc = { CDeclStruct.Name = name; Fields = fields }
        { env' with Decls = CDecl.Struct struc :: env'.Decls }, Some struc

and makeCDeclEnum env (typ: Type) : CEnv * (CDeclEnum option) =
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

        let enum = { CDeclEnum.Name = name; Consts = consts }
        { env with Decls = CDecl.Enum enum :: env.Decls }, Some enum

let makeCDeclGlobalVar env name typ : CEnv * (CDeclVar option) =
    let ctype = lookupCType env typ

    let var = { CDeclVar.Name = name; Type = ctype }
    { env with Decls = CDecl.GlobalVar var :: env.Decls }, Some var

let makeCDeclExtern env name typ =
    let ctype = lookupCType env typ

    let exter = { CDeclExtern.Type = ctype; Name = name }
    { env with Decls = CDecl.Extern exter :: env.Decls }, Some exter

let makeCDeclStructs (env: CEnv) = function
    | [] -> env
    | funcs -> 
        funcs 
        |> List.fold (fun env x -> 
            let env, _ = makeCDeclStruct env x
            env) env

let makeCDeclEnums (env: CEnv) = function
    | [] -> env
    | enums ->
        enums
        |> List.fold (fun env x ->
            match tryLookupCType env x with
            | Some _ -> env
            | _ ->
                let env, _ = makeCDeclEnum env x
                env) env

let makeCDeclFunctions (env: CEnv) = function
    | [] -> env
    | funcs ->
        funcs 
        |> List.fold (fun env x -> 
            let env, _ = makeCDeclFunction env x
            env) env

let makeCDeclFunctionPrototypes (env: CEnv) = function
    | [] -> env
    | funcs ->
        funcs 
        |> List.fold (fun env x -> 
            let env, _ = makeCDeclFunctionPrototype env x
            env) env

let makeCDeclFunctionPointers (env: CEnv) = function
    | [] -> env
    | funcPtrs ->
        funcPtrs
        |> List.fold (fun env x -> 
            let env, _ = makeCDeclFunctionPointer env x
            env) env

let makeCDeclGlobalVars (env: CEnv) = function
    | [] -> env
    | (types : Type list) ->
        types
        |> List.fold (fun env x ->
            match x.Name.Contains("__ferop_exported__") with
            | true ->
                let name = sprintf "%s_%s" env.Name (x.Name.Replace ("__ferop_exported__", ""))
                let env, _ = makeCDeclGlobalVar env name x
                env
            | _ ->
                let name = sprintf "%s_%s" env.Name x.Name
                let env, _ = makeCDeclGlobalVar env name x
                env
        ) env
        
let makeCDeclExterns (env: CEnv) = function
    | [] -> env
    | (types : Type list) ->
        types
        |> List.fold (fun env x ->
            match x.Name.Contains("__ferop_exported__") with
            | true ->
                let name = sprintf "%s_%s" env.Name (x.Name.Replace ("__ferop_exported__", ""))
                let env, _ = makeCDeclExtern env name x
                env
            | _ ->
                let name = sprintf "%s_%s" env.Name x.Name
                let env, _ = makeCDeclExtern env name x
                env
        ) env

let makeCDecls (env: CEnv) info =
    let funcs = info.ImportedFunctions

    if funcs.IsEmpty then env
    else

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
        |> List.filter (fun x -> x.Name.Contains("__ferop_exported__"))

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
    { env''''''' with Decls = env'''''''.Decls |> List.rev }

//-------------------------------------------------------------------------
// CEnv
//-------------------------------------------------------------------------

let makeCEnv info =
    let env = makeEmptyEnv info.Name
    makeCDecls { env with IsCpp = info.IsCpp } info
