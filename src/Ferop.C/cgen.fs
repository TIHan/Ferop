module Ferop.CGen

open Ferop.CConversion
open Ferop.CTypedAST

type CGen = {
    Header : string
    Source : string }

let generateHeaderCode (name : string) header =
    let name = name.ToUpper ()
    sprintf """
#ifndef __%s_H__
#define __%s_H__
%s
#endif
""" 
        name name header

let generateMainHeaderCode name header =
    generateHeaderCode name <| sprintf
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
"           header

let generateCDeclFunctionPrototypeCode returnType name parameterTypes =
    sprintf """
FEROP_EXPORT %s FEROP_DECL %s (%s);
"""
        returnType name parameterTypes

let generateCDeclFunctionCode = 
    sprintf """
FEROP_EXPORT %s FEROP_DECL %s (%s)
{
%s 
}
"""

let generateCDeclFunctionPointerCode returnType name parameters =
    sprintf """
typedef %s (*%s)(%s);
extern %s Delegate_%s;
FEROP_EXPORT void FEROP_DECL ferop_set_%s (%s);
"""
        returnType name parameters name name name name

let genereateCDeclFunctionPointerImplCode name =
    sprintf """
%s Delegate_%s;
FEROP_EXPORT void FEROP_DECL ferop_set_%s (%s ptr)
{
    Delegate_%s = ptr;
}
"""
        name name name name name

let generateCDeclStructCode fields name =
    sprintf """
typedef struct {
%s
} %s;
"""
        fields name

let generateHeaderIncludeCode name = sprintf "#include \"%s.h\" \n" name

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

let generateCField {CField.Type=typ; Name=name} =
    let ctype = generateCType typ
    sprintf "%s %s;\n" ctype (name.Replace (" ", "_"))

let generateCParameter {CParameter.Type=typ; Name=name} =
    let ctype = generateCType typ
    sprintf "%s %s" ctype (name.Replace (" ", "_"))

let generateCFields = function
    | [] -> ""
    | fields ->
        
    fields
    |> List.map generateCField
    |> List.reduce (fun x y -> x + y)

let generateReturnType = function
    | None -> "void"
    | Some x -> generateCType x

let generateParameters = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map generateCParameter
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

    generateCDeclFunctionCode returnType' name parameters' body

let generateCDeclFunctionPointer {CDeclFunctionPointer.ReturnType=returnType; Name=name; ParameterTypes=parameterTypes} =
    let returnType' = generateReturnType returnType
    let parametersTypes' = generateParameterTypes parameterTypes

    generateCDeclFunctionPointerCode returnType' name parametersTypes'

let generateCDeclStruct {CDeclStruct.Name=name; Fields=fields} =
    generateCDeclStructCode (generateCFields fields) name

let generateCDecl = function
    | Function x ->         generateCDeclFunction x
    | FunctionPointer x ->  generateCDeclFunctionPointer x
    | Struct x ->           generateCDeclStruct x

let generateCDeclFunctionPointerImpl {CDeclFunctionPointer.Name=name} =
    genereateCDeclFunctionPointerImplCode name

let generateCDeclFunctions = function
    | [] -> ""
    | funcs -> 

    funcs
    |> List.map generateCDeclFunction
    |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclStructs = function
    | [] -> ""
    | structs -> 

    structs
    |> List.map generateCDeclStruct
    |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclFunctionPointers = function
    | [] -> ""
    | funcs -> 

    funcs
    |> List.map generateCDeclFunctionPointer
    |> List.reduce (fun x y -> x + "\n" + y)

let generateCDeclFunctionPointerImpls = function
    | [] -> ""
    | funcs -> 

    funcs
    |> List.map generateCDeclFunctionPointerImpl
    |> List.reduce (fun x y -> x + "\n" + y)

let generateHeader (env: CEnv) includes =
    let structs = generateCDeclStructs env.DeclStructs
    let funcPtrs = generateCDeclFunctionPointers env.DeclFunctionPointers

    sprintf "%s\n%s\n%s" includes structs funcPtrs
    |> generateMainHeaderCode env.Name

let generateSource (env: CEnv) =
    (generateHeaderIncludeCode env.Name) + 
    generateCDeclFunctions env.DeclFunctions +
    generateCDeclFunctionPointerImpls env.DeclFunctionPointers

let generate env includes =
    { Header = generateHeader env includes; Source = generateSource env }