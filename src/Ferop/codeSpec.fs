module internal Ferop.CodeSpec

open System

open Ferop.Code
open Ferop.Core
open Ferop.Helpers

type CodeSpec =
    {
    Includes: string
    FunctionName: string
    ReturnType: Type
    Parameters: Parameter list
    Body: string }

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
    (typeof<string>,            "const char *")
    (typeof<nativeptr<byte>>,   "uint8_t *")
    (typeof<nativeptr<sbyte>>,  "int8_t *")
    (typeof<nativeptr<single>>, "float *")
    (typeof<nativeptr<double>>, "double *")]

let defaultCode =
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

let findReturnType typ =
    match
        returnTypes
        |> List.tryFind (fun (x, _) -> x = typ)
        with
    | None -> failwith "Invalid return type."
    | Some (_, x) -> x

let findParameterType typ =
    match
        parameterTypes
        |> List.tryFind (fun (x, _) -> x = typ)
        with
    | None -> failwith "Invalid parameter type."
    | Some (_, x) -> x

let generateParameters = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map (fun ({ Name = name; Type = typ }) ->
        let ctype = findParameterType typ
        sprintf "%s %s" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> sprintf "%s, %s" x y)

let generateCode codeSpec =
        sprintf """
%s
%s

FEROP_EXPORT %s FEROP_DECL %s (%s)
{
    %s
}
"""
            defaultCode
            codeSpec.Includes
            (findReturnType codeSpec.ReturnType)
            codeSpec.FunctionName
            (generateParameters codeSpec.Parameters)
            codeSpec.Body

