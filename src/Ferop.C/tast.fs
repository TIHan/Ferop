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
    | Pointer of CType
    | Array of CType * size: int option
    | Struct of CStruct

and CStruct = { Name: string; Fields: CField list }

and CField = CField of CType * name: string

type CLocalVar = CLocalVar of CType * name: string

type CDecl =
    | Function of returnType: CType option * name: string * parameters: CLocalVar list * CExpr

type CEnv = { Decls : CDecl list; StructMap: Map<string, CStruct> }

let makeEmptyEnv () = { Decls = List.empty; StructMap = Map.empty }