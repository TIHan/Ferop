[<RequireQualifiedAccess>]
module internal Ferop.Osx

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

open Ferop.Code
open Ferop.Core
open Ferop.Helpers
open Ferop.CodeSpec

open FSharp.Control.IO

let makeDllName modul = sprintf "lib%s.dylib" modul.Name

let makeCFilePath path name = Path.Combine (path, sprintf "%s.c" name)

let makeOFilePath path name = Path.Combine (path, sprintf "%s.o" name)

let makeStaticLibraryPath path name = Path.Combine (path, sprintf "lib%s.a" name)

let makeDynamicLibraryPath path name = Path.Combine (path, sprintf "lib%s.dylib" name)

let checkProcessError (p: Process) = if p.ExitCode <> 0 then failwith (p.StandardError.ReadToEnd ())

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

let makeCodeSpec includes = function
    | Inline { Name = name; ReturnType = returnType; Parameters = parameters; Code = code } ->
        {
        Includes = includes
        FunctionName = name
        ReturnType = returnType
        Parameters = parameters
        Body = code }
    | Extern _ ->
        failwith "not supported"

let compileFunction (path: string) (tb: TypeBuilder) (modul: Module) (func: Function) = io {
    let codeSpec = makeCodeSpec (modul.Includes) func

    let name = codeSpec.FunctionName
    let returnType = codeSpec.ReturnType
    let parameters = codeSpec.Parameters
    let cFile = makeCFilePath path name
    let oFile = makeOFilePath path name

    let flags = modul.ClangFlagsOsx
    let dllName = makeDllName modul

    let code = generateCode codeSpec

    let! pinvokeMeth = definePInvokeMethod name dllName name returnType parameters tb

    return! compileC flags cFile oFile code }

let compileToStaticLibrary aFile oFiles = io {
    let args = sprintf "rcs %s %s" aFile oFiles
    do! startAr args }

let compileToDynamicLibrary libs oFiles dylibName = io {
    let args = sprintf "-arch i386 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 %s %s -o %s " libs oFiles dylibName
    do! startClang args }

let compileFunctions path tb modul = io {
    let! functions = modul.Functions |> List.map (compileFunction path tb modul)
    return List.reduce (fun x y -> sprintf "%s %s" x y) functions }

let cleanObjectFiles path = io {
    return
        Directory.GetFiles (path, "*.o")
        |> Array.iter (fun x -> File.Delete x) }

let compileModule (path: string) (tb: TypeBuilder) (modul: Module) =
    let dylibName = makeDynamicLibraryPath path modul.Name

    let libs = modul.ClangLibsOsx
    io {
        let! oFiles = compileFunctions path tb modul
        do! compileToDynamicLibrary libs oFiles dylibName
        return! cleanObjectFiles path }
    |> IO.run

    tb.CreateType ()
