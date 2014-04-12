module Ferop.CGen

open Ferop.CConversion
open Ferop.CTypedAST

type CGen = {
    Header : string
    Source : string }

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

let generateCDeclStructf =
    sprintf """
typedef struct {
%s
} %s;
"""

let generateCDeclFunctionf = sprintf """
FEROP_EXPORT %s FEROP_DECL %s (%s)
{
%s 
}
"""

let makeHeaderInclude name = sprintf "#include \"%s.h\" \n" name

let rec generateCType = function
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
    | Pointer None -> "void*"
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

let generateCDeclStruct = function
    | CDecl.Struct (name, fields) -> 
        generateCDeclStructf (generateCFields fields) name
    | _ -> failwith "Expected a struct decl"

let generateCDeclStructs = function
    | [] -> ""
    | structs ->

    structs
    |> List.sortWith (fun x -> function
        | CDecl.Struct (name1, fields) ->
            if fields |> List.exists (fun (CField(typ,_)) -> 
                match typ with
                | Struct (CStruct (name2, _)) -> name1 = name2
                | _ -> false) then
                0
            else
                1
        | _ -> 0)
    |> List.map (fun x -> generateCDeclStruct x)
    |> List.reduce (fun x y -> x + "\n\n" + y)       

let generateReturnType = function
    | None -> "void"
    | Some x -> generateCType x

let generateParameters = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map (fun (typ, name: string) ->
            let ctype = generateCType typ
            sprintf "%s %s" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> sprintf "%s, %s" x y)

let generateCExpr = function
    | Text x -> x

let generateCDecl = function
    | CDecl.Function (returnType, name, parameters, expr) ->
        let returnType' = generateReturnType returnType
        let parameters' = generateParameters parameters
        let body = generateCExpr expr

        generateCDeclFunctionf returnType' name parameters' body
    | _ -> ""

let generateHeader env includes =
    let structDefs = generateCDeclStructs (env.Decls |> List.filter (function | CDecl.Struct _ -> true | _ -> false))
    generateMainHeaderf env.Name <|
        sprintf "%s\n\n%s" includes structDefs 

let generateSource env =
    (makeHeaderInclude env.Name) +
    (env.Decls
    |> List.map generateCDecl
    |> List.reduce (fun x y -> x + "\n\n" + y))

let generate env includes =
    { Header = generateHeader env includes; Source = generateSource env }