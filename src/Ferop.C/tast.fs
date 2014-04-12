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
    | Array of CType * size: int option
    | Struct of CStruct
    | Function of CFunction

and CStruct = CStruct of name: string * fields: CField list

and CField = CField of CType * name: string

and CFunction = CFunction of returnType: CType option * name: string * parameterTypes: CType list

and 
    [<RequireQualifiedAccess>]
    CDecl =
    | Function of returnType: CType option * name: string * parameters: (CType * string) list * CExpr
    | Struct of name: string * fields: CField list

type CEnv = { 
    Name : string
    Decls : CDecl list
    Structs : CStruct list }

let makeEmptyEnv name = { Name = name; Decls = List.empty; Structs = List.empty }