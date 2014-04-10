module Ferop.CTypedAST

// This is a micro micro micro typed ast. Only handles types and function declarations.

// We are not handling full-blown c expressions. We only have the actual text.
type CExpr =
    | Text of string

type CType =
    | Void
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
    | Pointer of CType
    | Array of CType * size: int option
    | Struct of CStruct

and CStruct = CStruct of name: string * fields: CField list

and CField = CField of CType * name: string

and CVar = CVar of CType * name: string

and CDecl =
    | Function of returnType: CType option * name: string * parameters: CVar list * CExpr

type CEnv = { 
    Name : string
    Decls : CDecl list
    Structs : CStruct list }

let makeEmptyEnv name = { Name = name; Decls = List.empty; Structs = List.empty }