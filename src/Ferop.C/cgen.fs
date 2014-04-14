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

let generateCFunctionPointer returnType name parameters =
    sprintf """
typedef %s (*fs_%s)(%s);
extern fs_%s Fs_%s;
FEROP_EXPORT void FEROP_DECL ferop_set_fs_%s (fs_%s);
"""
        returnType name parameters name name name name

let genereateCFunctionPointerImpl name =
    sprintf """
fs_%s Fs_%s;
FEROP_EXPORT void FEROP_DECL ferop_set_fs_%s (fs_%s ptr)
{
    Fs_%s = ptr;
}
"""
        name name name name name

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
    | Struct ({ Name = x }) -> x
    | x -> failwithf "%A generated type not found." x

let generateCFields = function
    | [] -> ""
    | fields ->
        
    fields
    |> List.map (function
        | { Type = typ; Name = name } ->
            let ctype = generateCType typ
            sprintf "%s %s;\n" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> x + y)

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

let generateCDeclFunctionPointer = function
    | CDecl.Function (returnType, name, parameters, _) ->
        let returnType' = generateReturnType returnType
        let parameters' = generateParameters parameters

        generateCFunctionPointer returnType' name parameters'
    | _ -> ""

let generateCDeclFunctionPointerImpl = function
    | CDecl.Function (_, name, _, _) ->
        genereateCFunctionPointerImpl name
    | _ -> ""

let generateCDeclPrototypes = function
    | [] -> ""
    | decls -> List.map generateCDeclPrototype decls |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclFunctions = function
    | [] -> ""
    | funcs -> List.map generateCDecl funcs |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclStructs = function
    | [] -> ""
    | structs -> List.map generateCDecl structs |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclFunctionPointers = function
    | [] -> ""
    | funcs -> List.map generateCDeclFunctionPointer funcs |> List.reduce (fun x y -> x + "\n" + y)

let genereateCDeclFunctionPointerImpls = function
    | [] -> ""
    | funcs -> List.map generateCDeclFunctionPointerImpl funcs |> List.reduce (fun x y -> x + "\n" + y)

let generateHeader env includes =
    let prototypes = generateCDeclPrototypes env.Decls
    let structs = generateCDeclStructs env.DeclStructs
    generateMainHeaderf env.Name <|
        sprintf "%s\n%s\n%s" includes structs prototypes

let generateSource (env: CEnv) =
    (generateHeaderInclude env.Name) + 
    generateCDeclFunctions env.DeclFunctions

let generate env includes =
    { Header = generateHeader env includes; Source = generateSource env }