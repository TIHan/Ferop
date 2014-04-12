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

and 
    [<RequireQualifiedAccess>]
    CDecl =
    | Function of returnType: CType option * name: string * parameters: (CType * string) list * CExpr
    | Struct of name: string * fields: CField list

let hasFieldType fieldTypeName = function
    | [] -> false
    | fields ->

    fields
    |> List.exists (function | {Type = Struct x} -> x.Name = fieldTypeName | _ -> false)

type CEnv = { 
    Name : string
    Decls : CDecl list } with

    member this.DeclFunctions = 
        this.Decls
        |> List.filter (function | CDecl.Function _ -> true | _ -> false)

    member this.DeclStructs = 
        this.Decls 
        |> List.filter (function | CDecl.Struct _ -> true | _ -> false)
        |> List.sortWith (fun x y ->
            match (x, y) with
            | (CDecl.Struct (_, fields)), (CDecl.Struct (name, _)) -> 
                    match hasFieldType name fields with
                    | true -> 1
                    | _ -> 0
            | _ -> 0)

let makeEmptyEnv name = { Name = name; Decls = List.empty }