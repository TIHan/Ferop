module Ferop.CGen

open Ferop.CConversion
open Ferop.CTypedAST

type CGen = {
    Header : string
    Body : string }

let generateHeaderf (name : string) =
    sprintf """
#ifndef __%s_H__
#define __%s_H__
%s
#endif
""" 
        (name.ToUpper ())
        (name.ToUpper ())

let generateMainHeaderf name body =
    generateHeaderf name <| sprintf
        @"
#include <stdint.h>

#if defined(_WIN32)
#   define FEROP_IMPORT       __declspec(dllimport)
#   define FEROP_EXPORT       __declspec(dllexport)
#   define FEROP_DECL         __cdecl
#elif defined(__GNUC__)
#   define FEROP_EXPORT       __attribute__((visibility(""default"")))
#   define FEROP_IMPORT
#   define FEROP_DECL         __attribute__((cdecl))
#else
#   error Compiler not supported.
#endif
%s
"           body

let generateCStructf =
    sprintf """
typedef struct {
%s
} %s;"""

let generateCDeclf = sprintf """FEROP_EXPORT %s FEROP_DECL %s (%s)
{
%s 
}"""

let makeHeaderInclude name = sprintf "#include \"%s.h\" \n" name

let rec generateCType = function
    | Void ->   "void"
    | Byte ->   "uint8_t"
    | SByte ->  "int8_t"
    | UInt16 -> "uint16_t"
    | Int16 ->  "int16_t"
    | UInt32 -> "uint32_t"
    | Int32 ->  "int32_t"
    | UInt64 -> "uint64_t"
    | Int64 ->  "int64_t"
    | Float ->  "float"
    | Double -> "double"
    | Pointer x -> generateCType x + "*"
    | Struct (CStruct (name=x)) -> x
    | x -> failwithf "%A generated type not found." x

let generateCFields = function
    | [] -> ""
    | fields ->
        
    fields
    |> List.map (function
        | CField (typ, name) ->
            let ctype = generateCType typ
            sprintf "%s %s" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> sprintf "%s;\n%s;" x y)

let generateCStruct (CStruct (name, fields)) = generateCStructf (generateCFields fields) name

let generateCStructs = function
    | [] -> ""
    | structs ->

    structs
    |> List.sortWith (fun x (CStruct(fields=fields)) ->
        if fields |> List.exists (fun (CField(typ,_)) -> 
            match typ with
            | Struct y -> x = y
            | _ -> false) then
            0
        else
            1)
    |> List.map (fun x -> generateCStruct x)
    |> List.reduce (fun x y -> x + "\n\n" + y)       

let generateReturnType = function
    | None -> "void"
    | Some x -> generateCType x

let generateParameters = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map (function
        | CVar (typ, name) ->
            let ctype = generateCType typ
            sprintf "%s %s" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> sprintf "%s, %s" x y)

let generateCExpr = function
    | Text x -> x

let generateCDecl = function
    | Function (returnType, name, parameters, expr) ->
        let returnType' = generateReturnType returnType
        let parameters' = generateParameters parameters
        let body = generateCExpr expr

        generateCDeclf returnType' name parameters' body

let generateHeader env includes =
    let structDefs = generateCStructs env.Structs
    generateMainHeaderf env.Name <|
        sprintf "%s\n\n%s" includes structDefs 

let generateBody env =
    (makeHeaderInclude env.Name) +
    (env.Decls
    |> List.map generateCDecl
    |> List.reduce (fun x y -> x + "\n\n" + y))

let generate env includes =
    { Header = generateHeader env includes; Body = generateBody env }