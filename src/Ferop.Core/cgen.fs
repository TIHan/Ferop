module internal Ferop.CGeneration

open CTypedAST

type CGen = {
    Header : string
    Source : string }

type CGenContext =
    | Header = 0
    | Source = 1

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
"""
        returnType name parameters

let generateCDeclStructCode fields name =
    sprintf """
typedef struct {
%s
} %s;
"""
        fields name

let generateCDeclEnumCode consts name =
    sprintf """
typedef enum {
%s
} %s;
"""
        consts name

let generateCDeclVarCode typ name = sprintf """%s %s;""" typ name

let generateCDeclExternCode typ name =
    sprintf """
extern %s %s;
"""
        typ name

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
    | Pointer (Some x) -> generateCType x + "*"
    | CType.Struct {Name=name} -> name
    | CType.Function {Name=name} -> name
    | CType.Enum {Name=name} -> name
    | x -> failwithf "%A generated type not found." x

let generateCField {CField.Type=typ; Name=name} =
    let ctype = generateCType typ
    sprintf "%s %s;\n" ctype (name.Replace (" ", "_"))

let generateCParameter {CParameter.Type=typ; Name=name} =
    let ctype = generateCType typ
    sprintf "%s %s" ctype (name.Replace (" ", "_"))

let generateCEnumConst {CEnumConst.Name=name; Value=value} =
    sprintf "%s = %i," name value

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

let generateCEnumConsts = function
    | [] -> ""
    | consts ->

    consts
    |> List.map generateCEnumConst
    |> List.reduce (fun x y -> sprintf "%s\n%s" x y)

let generateCExpr = function
    | Text x -> x

let generateCDeclFunction {ReturnType=returnType; Name=name; Parameters=parameters; Expr=expr} =
    let returnType' = generateReturnType returnType
    let parameters' = generateParameters parameters
    let body = generateCExpr expr

    generateCDeclFunctionCode returnType' name parameters' body

let generateCDeclFunctionPrototype {CDeclFunctionPrototype.ReturnType=returnType; Name=name; ParameterTypes=parameterTypes} =
    let returnType' = generateReturnType returnType
    let parametersTypes' = generateParameterTypes parameterTypes

    generateCDeclFunctionPrototypeCode returnType' name parametersTypes'

let generateCDeclFunctionPointer {CDeclFunctionPointer.ReturnType=returnType; Name=name; ParameterTypes=parameterTypes} =
    let returnType' = generateReturnType returnType
    let parametersTypes' = generateParameterTypes parameterTypes

    generateCDeclFunctionPointerCode returnType' name parametersTypes'

let generateCDeclStruct {CDeclStruct.Name=name; Fields=fields} =
    generateCDeclStructCode (generateCFields fields) name

let generateCDeclEnum {CDeclEnum.Name=name; Consts=consts} =
    generateCDeclEnumCode (generateCEnumConsts consts) name

let generateCDeclVar {CDeclVar.Name=name; Type=typ} =
    generateCDeclVarCode (generateCType typ) name

let generateCDeclExtern {CDeclExtern.Type=typ; Name=name} =
    generateCDeclExternCode (generateCType typ) name

let generateCDecl context = function
    | Function x when context = CGenContext.Source ->
        generateCDeclFunction x
    | FunctionPrototype x when context = CGenContext.Header ->
        generateCDeclFunctionPrototype x
    | FunctionPointer x when context = CGenContext.Header ->
        generateCDeclFunctionPointer x
    | Struct x when context = CGenContext.Header ->
        generateCDeclStruct x
    | Enum x when context = CGenContext.Header ->
        generateCDeclEnum x
    | GlobalVar x when context = CGenContext.Source ->
        generateCDeclVar x
    | Extern x when context = CGenContext.Header ->
        generateCDeclExtern x
    | _ -> ""

let generateCDecls context = function
    | [] -> ""
    | decls ->

    match
        decls
        |> List.map (generateCDecl context)
        |> List.filter ((<>)"") with
    | [] -> ""
    | decls -> decls |> List.reduce (fun x y -> x + "\n" + y)

let generateHeader env extra =
    let decls = generateCDecls CGenContext.Header env.Decls

    (sprintf "%s\n%s\n%s\n%s" extra)
        (
            if env.IsCpp
            then """extern "C" {"""
            else ""
        ) 
        decls
        (
            if env.IsCpp 
            then """}"""
            else ""
        ) 
    |> generateMainHeaderCode env.Name

let generateSource (env: CEnv) extra =
    (generateHeaderIncludeCode env.Name) +
    extra + "\n" +
    (
        if env.IsCpp
        then """extern "C" {"""
        else "" 
    )
    + generateCDecls CGenContext.Source env.Decls +
    (
        if env.IsCpp
        then """}"""
        else ""
    )

let generate env extraHeader extraSource =
    { Header = generateHeader env extraHeader; Source = generateSource env extraSource }