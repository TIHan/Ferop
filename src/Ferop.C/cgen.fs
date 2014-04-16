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
typedef %s (*%s)(%s);
extern %s Delegate_%s;
FEROP_EXPORT void FEROP_DECL ferop_set_%s (%s);
"""
        returnType name parameters name name name name

let genereateCFunctionPointerImpl name =
    sprintf """
%s Delegate_%s;
FEROP_EXPORT void FEROP_DECL ferop_set_%s (%s ptr)
{
    Delegate_%s = ptr;
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
    | CType.Struct {Name=name} -> name
    | x -> failwithf "%A generated type not found." x

let generateCFields = function
    | [] -> ""
    | fields ->
        
    fields
    |> List.map (function
        | {CField.Type=typ; Name=name} ->
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
    |> List.map (fun {CParameter.Type=typ; Name=name} ->
            let ctype = generateCType typ
            sprintf "%s %s" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> sprintf "%s, %s" x y)

let generateParameterTypes = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map generateCType
    |> List.reduce (fun x y -> sprintf "%s, %s" x y)

let generateCExpr = function
    | Text x -> x

let generateCDeclFunction {ReturnType=returnType; Name=name; Parameters=parameters; Expr=expr} =
    let returnType' = generateReturnType returnType
    let parameters' = generateParameters parameters
    let body = generateCExpr expr

    generateCFunctionf returnType' name parameters' body

let generateCDeclFunctionPointer {CDeclFunctionPointer.ReturnType=returnType; Name=name; ParameterTypes=parameterTypes} =
    let returnType' = generateReturnType returnType
    let parametersTypes' = generateParameterTypes parameterTypes

    generateCFunctionPointer returnType' name parametersTypes'

let generateCDeclStruct {CDeclStruct.Name=name; Fields=fields} =
    generateCStructf (generateCFields fields) name

let generateCDecl = function
    | Function x ->         generateCDeclFunction x
    | FunctionPointer x ->  generateCDeclFunctionPointer x
    | Struct x ->           generateCDeclStruct x

let generateCDeclFunctionPointerImpl {CDeclFunctionPointer.Name=name} =
    genereateCFunctionPointerImpl name

let generateCDeclFunctions = function
    | [] -> ""
    | funcs -> List.map generateCDeclFunction funcs |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclStructs = function
    | [] -> ""
    | structs -> List.map generateCDeclStruct structs |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclFunctionPointers = function
    | [] -> ""
    | funcs -> List.map generateCDeclFunctionPointer funcs |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclFunctionPointerImpls = function
    | [] -> ""
    | funcs -> List.map generateCDeclFunctionPointerImpl funcs |> List.reduce (fun x y -> x + "\n" + y)

let generateHeader (env: CEnv) includes =
    let structs = generateCDeclStructs env.DeclStructs
    let funcPtrs = generateCDeclFunctionPointers env.DeclFunctionPointers
    generateMainHeaderf env.Name <|
        sprintf "%s\n%s\n%s" includes structs funcPtrs

let generateSource (env: CEnv) =
    (generateHeaderInclude env.Name) + 
    generateCDeclFunctions env.DeclFunctions +
    generateCDeclFunctionPointerImpls env.DeclFunctionPointers

let generate env includes =
    { Header = generateHeader env includes; Source = generateSource env }