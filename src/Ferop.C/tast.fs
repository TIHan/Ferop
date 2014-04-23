module Ferop.CTypedAST

// This is a micro micro micro typed ast. Only handles types and function declarations.

// We are not handling full-blown c expressions. We only have the actual text.
type CExpr =
    | Text of string

type CType =
    | Byte
    | SByte
    | UInt16
    | Int16
    | UInt32
    | Int32
    | UInt64
    | Int64
    | Float
    | Double
    | Pointer of CType option
    | Array of CArray
    | Struct of CStruct
    | Function of CFunction

and CArray = {
    Type: CType
    Size: int option }

and CStruct = { 
    Name: string
    Fields: CField list }

and CField = {
    Type: CType
    Name: string }

and CFunction = {
    ReturnType: CType option
    Name: string 
    ParameterTypes: CType list }

and CParameter = {
    Type: CType
    Name: string }

and CDeclFunction = {
    ReturnType: CType option
    Name: string
    Parameters: CParameter list
    Expr: CExpr }

and CDeclFunctionPrototype = {
    ReturnType: CType option
    Name: string
    ParameterTypes: CType list }

and CDeclFunctionPointer = {
    ReturnType: CType option
    Name: string
    ParameterTypes: CType list }

and CDeclStruct = {
    Name: string
    Fields: CField list }

and CDeclExtern = {
    Name: string
    Type: CType }

and 
    CDecl =
    | Function of CDeclFunction
    | FunctionPrototype of CDeclFunctionPrototype
    | FunctionPointer of CDeclFunctionPointer
    | Struct of CDeclStruct
    | Extern of CDeclExtern

let hasFieldType fieldTypeName = function
    | [] -> false
    | fields ->

    fields
    |> List.exists (function | {CField.Type = CType.Struct x} -> x.Name = fieldTypeName | _ -> false)

type CEnv = { 
    Name : string
    Decls : CDecl list } with

    member this.DeclFunctions = 
        this.Decls
        |> List.choose (function | CDecl.Function x -> Some x | _ -> None)

    member this.DeclFunctionPointers =
        this.Decls
        |> List.choose (function | CDecl.FunctionPointer x -> Some x | _ -> None)

    member this.DeclStructs = 
        this.Decls
        |> List.choose (function | CDecl.Struct x -> Some x | _ -> None)

let makeEmptyEnv name = { Name = name; Decls = List.empty }