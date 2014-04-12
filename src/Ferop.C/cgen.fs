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

let generateCFunctionPrototypef =
    sprintf """
FEROP_EXPORT %s FEROP_DECL %s (%s);
"""

let generateCFunctionf = 
    sprintf """
FEROP_EXPORT %s FEROP_DECL %s (%s)
{
%s 
}
"""

let generateCStructf =
    sprintf """
typedef struct {
%s
} %s;
"""

let generateHeaderInclude name = sprintf "#include \"%s.h\" \n" name

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

let generateCDeclPrototype = function
    | CDecl.Function (returnType, name, parameters, _) ->
        let returnType' = generateReturnType returnType
        let parameters' = generateParameters parameters

        generateCFunctionPrototypef returnType' name parameters'
    | _ -> ""

let generateCDecl = function
    | CDecl.Function (returnType, name, parameters, expr) ->
        let returnType' = generateReturnType returnType
        let parameters' = generateParameters parameters
        let body = generateCExpr expr

        generateCFunctionf returnType' name parameters' body
    | CDecl.Struct (name, fields) -> 
        generateCStructf (generateCFields fields) name

let generateHeader env includes =
    let prototypes = List.map generateCDeclPrototype env.Decls |> List.reduce (fun x y -> x + "\n" + y)
    let structs = List.map generateCDecl env.DeclStructs |> List.reduce (fun x y -> x + "\n" + y)
    generateMainHeaderf env.Name <|
        sprintf "%s\n%s\n%s" includes structs prototypes

let generateSource (env: CEnv) =
    (generateHeaderInclude env.Name) +
    (env.DeclFunctions
    |> List.map generateCDecl
    |> List.reduce (fun x y -> x + "\n\n" + y))

let generate env includes =
    { Header = generateHeader env includes; Source = generateSource env }