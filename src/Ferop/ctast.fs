module internal Ferop.CTypedAST

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
    | Enum of CEnum
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

and CEnum = {
    Name: string }

and CEnumConst = {
    Name: string
    Value: int }

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

and CDeclEnum = {
    Name: string
    Consts: CEnumConst list }

and CDeclVar = {
    Type: CType
    Name: string }

and CDeclExtern = {
    Type: CType
    Name: string }

and 
    CDecl =
    | Function of CDeclFunction
    | FunctionPrototype of CDeclFunctionPrototype
    | FunctionPointer of CDeclFunctionPointer
    | Struct of CDeclStruct
    | Enum of CDeclEnum
    | GlobalVar of CDeclVar
    | Extern of CDeclExtern

type CEnv = { 
    Name: string
    Decls: CDecl list
    IsCpp: bool }

let makeEmptyEnv name = { Name = name; Decls = List.empty; IsCpp = false }