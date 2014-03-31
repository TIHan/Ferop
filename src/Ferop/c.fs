[<RequireQualifiedAccess>]
module internal Ferop.C

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Ferop.Code
open Ferop.Core
open Ferop.Helpers

type IO<'a> = private IO of (unit -> 'a)
 
type IOBuilder () =
    member inline this.Bind (IO x : IO<'a>, f: 'a -> IO<'b>) : IO<'b> = 
        f (x ())

    member inline this.Bind (x: IO<'a> list, f: 'a list -> IO<'b>) : IO<'b> =
        f (x |> List.map (fun (IO x) -> x ()))

    member inline this.Delay (f: unit -> IO<'a>) : IO<'a> = 
        IO (fun _ -> match f () with | IO x -> x ())

    member inline this.Return (x: 'a) : IO<'a> =
        IO (fun _ -> x)

    member inline this.ReturnFrom (IO x : IO<'a>) : IO<'a> =
        IO (fun _ -> x ())

    member inline this.Zero () : IO<unit> =
        IO (fun _ -> ())
 
let io = IOBuilder ()

[<RequireQualifiedAccess>]
module IO =
    let inline run (IO x) = x ()

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

let includes (moduleType: Type) =
    let attrs =
        moduleType.CustomAttributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<IncludeAttribute>)
    attrs
    |> Seq.map (fun x -> Seq.exactlyOne x.ConstructorArguments)
    |> Seq.map (fun x -> "#include " + (x.Value :?> string))
    |> Seq.reduce (fun x y -> x + "\n" + y)

let rec findBody = function
    | Value (value, _) ->
        value.ToString ()
    | SpecificCall <@ C @> (expr, types, exprList) ->
        findBody exprList.[0]
    | Call (_, _, exprList) ->
        findBody exprList.[0]
    | Lambda (_, body) ->
        findBody body
    | x -> failwith "Invalid interop."

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
    | None ->
        match typ with
        | _ when typ.IsValueType -> sprintf "%s" typ.Name
        | _ when typ.IsClass -> sprintf "%s *" typ.Name
        | _ -> "Invalid parameter type."
    | Some (_, x) -> x

let generateParameters = function
    | [] -> ""
    | parameters ->

    parameters
    |> List.map (fun (Parameter (name, typ)) ->
        let ctype = findParameterType typ
        sprintf "%s %s" ctype (name.Replace (" ", "_")))
    |> List.reduce (fun x y -> sprintf "%s, %s" x y)

let generateCode includes name returnType parameters body =
        sprintf """
%s
%s

FEROP_EXPORT %s FEROP_DECL %s (%s)
{
    %s
}
"""          defaultCode includes (findReturnType returnType) name (generateParameters parameters) body

let methodExpr meth =
    match Expr.TryGetReflectedDefinition meth with
    | None -> failwithf "Reflected definition for %s not found" meth.Name
    | Some expr -> expr

let definePInvokeMethod name dllName entryName returnType (parameters: ParameterInfo []) (tb: TypeBuilder) = io {
    let meth = 
        tb.DefinePInvokeMethod (
            name,
            dllName,
            entryName,
            MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PinvokeImpl,
            CallingConventions.Standard,
            returnType,
            parameters |> Array.map (fun x -> x.ParameterType),
            CallingConvention.Cdecl,
            CharSet.Ansi)

    meth.SetImplementationFlags (meth.GetMethodImplementationFlags () ||| MethodImplAttributes.PreserveSig)
    return meth }

[<RequireQualifiedAccess>]
module Osx =
    let flags (moduleType: Type) =
        let attrs =
            moduleType.CustomAttributes
            |> Seq.filter (fun x -> x.AttributeType = typeof<ClangFlagsOsxAttribute>)
        let attr = Seq.exactlyOne attrs
        let args = Seq.exactlyOne attr.ConstructorArguments
        args.Value :?> string

    let libs (moduleType: Type) =
        let attrs =
            moduleType.CustomAttributes
            |> Seq.filter (fun x -> x.AttributeType = typeof<ClangLibsOsxAttribute>)
        let attr = Seq.exactlyOne attrs
        let args = Seq.exactlyOne attr.ConstructorArguments
        args.Value :?> string

    let checkProcessError (p: Process) =
        if p.ExitCode <> 0 then failwith (p.StandardError.ReadToEnd ())

    let startClang args = io {
        let pinfo = ProcessStartInfo ("clang", args)

        pinfo.UseShellExecute <- false
        pinfo.RedirectStandardError <- true

        let p = Process.Start (pinfo)
        p.WaitForExit ()

        checkProcessError p }

    let startAr args = io {
        let pinfo = ProcessStartInfo ("ar", args)

        pinfo.UseShellExecute <- false
        pinfo.RedirectStandardError <- true

        let p = Process.Start (pinfo)
        p.WaitForExit ()

        checkProcessError p }

    let compileC flags cFile oFile code : IO<string> = io {
        File.WriteAllText (cFile, code)

        let args = sprintf "-Wall -std=c99 -arch i386 %s -c %s -o %s" flags cFile oFile
        do! startClang args

        File.Delete (cFile)
        return oFile }

    let compileFunction (path: string) (tb: TypeBuilder) (meth: MethodInfo) = io {
        let expr = methodExpr meth
        let moduleType = meth.DeclaringType
        let dllName = sprintf "lib%s.dylib" moduleType.FullName
        let returnType = meth.ReturnType
        let parameters = meth.GetParameters ()
        let name = sprintf "%s_%s" moduleType.Name meth.Name
        let cFile = Path.Combine (path, sprintf "%s.c" name)
        let oFile = Path.Combine (path, sprintf "%s.o" name)

        let flags = flags moduleType
        let includes = includes moduleType

        let code = generateCode includes name returnType (findParameters meth) (findBody expr)

        let! pinvokeMeth = definePInvokeMethod meth.Name dllName name returnType parameters tb

        return! compileC flags cFile oFile code }

    let compileToStaticLibrary aFile oFiles = io {
        let args = sprintf "rcs %s %s" aFile oFiles
        do! startAr args }

    let compileToDynamicLibrary libs oFiles dylibName = io {
        let args = sprintf "-arch i386 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 %s %s -o %s " libs oFiles dylibName
        do! startClang args }

    let compileFunctions path tb moduleType = io {
        let! functions = Type.moduleFunctions moduleType |> List.map (compileFunction path tb)
        return List.reduce (fun x y -> sprintf "%s %s" x y) functions }

    let cleanObjectFiles path = io {
        return
            Directory.GetFiles (path, "*.o")
            |> Array.iter (fun x -> File.Delete x) }

    let compileModule (path: string) (tb: TypeBuilder) (moduleType: Type) =
        let staticlibName = Path.Combine (path, sprintf "lib%s.a" moduleType.FullName)
        let dylibName = Path.Combine (path, sprintf "lib%s.dylib" moduleType.FullName)

        let libs = libs moduleType
        io {
            let! oFiles = compileFunctions path tb moduleType
            do! compileToDynamicLibrary libs oFiles dylibName
            return! cleanObjectFiles path }
        |> IO.run

        tb.CreateType ()

let compileModule path tb moduleType =
        Osx.compileModule path tb moduleType
