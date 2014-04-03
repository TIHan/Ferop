module Ferop.CGen

open Ferop.C
open Ferop.CTypedAST

let generateHeaderf (name: string) =
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

let generateStructf =
    sprintf """
typedef struct {
%s
} %s;
"""

let generateCDeclf = sprintf """FEROP_EXPORT %s FEROP_DECL %s (%s)
{
%s 
}"""

let generateCType = function
    | Byte ->   "uint8_t"
    | SByte ->  "int8_t"
    | UInt16 -> "uint16_t"
    | Int16 ->  "int16_t"
    | UInt32 -> "uint32_t"
    | Int32 ->  "int32_t"
    | UInt64 -> "uint64_t"
    | Int64 ->  "int64_t"
    | x -> failwithf "%A generated type not found." x

let generateReturnType = function
    | None -> "void"
    | Some x -> generateCType x

let generateParameters = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map (function
        | CLocalVar (typ, name) ->
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