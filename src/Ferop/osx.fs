[<RequireQualifiedAccess>]
module internal Ferop.Osx

open System
open System.IO
open System.Diagnostics

open Ferop.Code
open Ferop.Core
open Ferop.Helpers
open Ferop.CodeSpec

open FSharp.Control.IO

let makeDllName modul = sprintf "lib%s.dylib" modul.Name

let makeCFilePath path codeSpec = Path.Combine (path, sprintf "%s.c" codeSpec.FunctionName)

let makeOFilePath path codeSpec = Path.Combine (path, sprintf "%s.o" codeSpec.FunctionName)

let makeStaticLibraryPath path modul = Path.Combine (path, sprintf "lib%s.a" modul.Name)

let makeDynamicLibraryPath path modul = Path.Combine (path, sprintf "lib%s.dylib" modul.Name)

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

let compileC flags cFile oFile code = io {
    File.WriteAllText (cFile, code)

    let args = sprintf "-Wall -std=c99 -arch i386 %s -c %s -o %s" flags cFile oFile
    do! startClang args

    File.Delete (cFile)
    return oFile }

/// Compiles a dummy c file that contains nothing. This ensures we at least get a dylib.
let compileDummyC flags = io {
    let cFile = "_ferop_dummy_.c"
    let oFile = "_ferop_dummy_.o"

    File.WriteAllText ("_ferop_dummy_.c", "")

    let args = sprintf "-Wall -std=c99 -arch i386 %s -c %s -o %s" flags cFile oFile
    do! startClang args

    File.Delete (cFile)
    return oFile }

let compileInlineFunction outputPath modul func definePInvoke = io {
    let dllName = makeDllName modul
    let flags = modul.ClangFlagsOsx

    let codeSpec = makeCodeSpec (modul.Includes) func

    let cFile = makeCFilePath outputPath codeSpec
    let oFile = makeOFilePath outputPath codeSpec

    let code = generateCode codeSpec

    do! definePInvoke dllName codeSpec

    return! compileC flags cFile oFile code }

let compileExternFunction modul func definePInvoke = io {
    let dllName = makeDllName modul
    let flags = modul.ClangFlagsOsx

    let codeSpec = makeCodeSpec (modul.Includes) func

    do! definePInvoke dllName codeSpec
    return "" }

let compileToStaticLibrary aFile oFiles = io {
    let args = sprintf "rcs %s %s" aFile oFiles
    do! startAr args }

let compileToDynamicLibrary libs oFiles dylibName = io {
    let args = sprintf "-arch i386 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 %s %s -o %s " libs oFiles dylibName
    do! startClang args }

let compileFunctions path modul definePInvoke = io {
    let! functions = modul.Functions |> List.map (function
        | Inline x as func ->
            compileInlineFunction path modul func definePInvoke
        | Extern x as func ->
            compileExternFunction modul func definePInvoke)
    let! dummy = compileDummyC modul.ClangFlagsOsx
    return List.reduce (fun x y -> sprintf "%s %s" x y) (dummy :: functions) }

let cleanObjectFiles outputPath = io {
    return
        Directory.GetFiles (outputPath, "*.o")
        |> Array.iter (fun x -> File.Delete x) }

let compileModule outputPath modul definePInvoke =
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.ClangLibsOsx

    io {
        let! oFiles = compileFunctions outputPath modul definePInvoke
        do! compileToDynamicLibrary libs oFiles dylibName
        return! cleanObjectFiles outputPath }
    |> IO.run
