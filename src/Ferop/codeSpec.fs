module internal Ferop.CodeSpec

open System

open Ferop.Code
open Ferop.Core
open Ferop.Helpers

type FunctionSpec = {
    Name: string
    ReturnType: Type
    Parameters: Parameter list
    Body: string }

type CodeSpec = {
    HeaderName: string
    FunctionSpec: FunctionSpec }

(*
type TypeSpec =
    | Standard of name: string
    | Custom of name: string * fields: FieldSpec list

and FieldSpec = { Name: string; TypeSpec: TypeSpec }
*)

let makeFunctionSpec = function
    | Inline { Name = name; ReturnType = returnType; Parameters = parameters; Code = code } ->
        {
        Name = name
        ReturnType = returnType
        Parameters = parameters
        Body = code }
    | Extern { Name = name; ReturnType = returnType; Parameters = parameters } ->
        {
        Name = name
        ReturnType = returnType
        Parameters = parameters
        Body = "" }

let makeCodeSpec headerName func =
    { HeaderName = headerName; FunctionSpec = makeFunctionSpec func }

let makeHeaderFileName name = sprintf "%s.h" name

let definePInvokeOfCodeSpec tb dllName funcSpec =
    definePInvokeMethod tb dllName funcSpec.Name funcSpec.Name funcSpec.ReturnType funcSpec.Parameters

// ********************************************
//      C
// ********************************************

let stdTypes =
    [
    (typeof<byte>,      "uint8_t")
    (typeof<sbyte>,     "int8_t")
    (typeof<uint16>,    "uint16_t")
    (typeof<int16>,     "int16_t")
    (typeof<uint32>,    "uint32_t")
    (typeof<int>,       "int32_t")
    (typeof<uint64>,    "uint64_t")
    (typeof<int64>,     "int64_t")
    (typeof<single>,    "float")
    (typeof<double>,    "double")]

let returnTypes =
    stdTypes @
    [
    (typeof<Void>,      "void")]

let parameterTypes =
    stdTypes @
    [
    (typeof<nativeptr<byte>>,   "uint8_t *")
    (typeof<nativeptr<sbyte>>,  "int8_t *")
    (typeof<nativeptr<uint16>>, "uint16_t *")
    (typeof<nativeptr<int16>>,  "int16_t *")
    (typeof<nativeptr<uint32>>, "uint32_t *")
    (typeof<nativeptr<int>>,    "int32_t *")
    (typeof<nativeptr<uint64>>, "uint64_t *")
    (typeof<nativeptr<int64>>,  "int64_t *")
    (typeof<nativeptr<single>>, "float *")
    (typeof<nativeptr<double>>, "double *")
    (typeof<nativeint>,         "void *")]

let defaultHeader =
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
    "

let headerf (name: string) =
    sprintf """
#ifndef __%s_H__
#define __%s_H__
%s
%s
#endif
""" 
        (name.ToUpper ())
        (name.ToUpper ())
        defaultHeader

let codef headerName =
    sprintf """
#include "%s"

FEROP_EXPORT %s FEROP_DECL %s (%s)
{
    %s
}
"""
        (makeHeaderFileName headerName)

let structf =
    sprintf """
typedef struct {
    %s
} %s;
"""

let findReturnType typ =
    match
        returnTypes
        |> List.tryFind (fun (x, _) -> x = typ)
        with
    | None ->
        match Type.isUnmanaged typ with
        | true -> typ.Name
        | _ -> failwith "Invalid return type."
    | Some (_, x) -> x

let findParameterType typ =
    match
        parameterTypes
        |> List.tryFind (fun (x, _) -> x = typ)
        with
    | None ->
        match Type.isUnmanaged typ with
        | true -> typ.Name
        | _ -> failwith "Invalid parameter type."
    | Some (_, x) -> x

let generateParameters = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map (fun ({ Name = name; Type = typ }) ->
        let ctype = findParameterType typ
        sprintf "%s %s" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> sprintf "%s, %s" x y)

//let makeTypeSpecStandard (typ: Type) = Standard (findParameterType typ)

let generateHeader name code =
    headerf name code

let generateCode codeSpec =
    codef
        codeSpec.HeaderName
        (findReturnType codeSpec.FunctionSpec.ReturnType)
        codeSpec.FunctionSpec.Name
        (generateParameters codeSpec.FunctionSpec.Parameters)
        codeSpec.FunctionSpec.Body
