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

let makeHeaderName modul = modul.ShortName

let makeHFilePath path modul = Path.Combine (path, makeHeaderFileName modul.ShortName)

let makeCFilePath path codeSpec = Path.Combine (path, sprintf "%s.c" codeSpec.FunctionName)

let makeOFilePath path codeSpec = Path.Combine (path, sprintf "%s.o" codeSpec.FunctionName)

let makeDummyCFilePath path = Path.Combine (path, "_ferop_dummy_.c")

let makeDummyOFilePath path = Path.Combine (path, "_ferop_dummy_.o")

let makeStaticLibraryPath path modul = Path.Combine (path, sprintf "lib%s.a" modul.Name)

let makeDynamicLibraryPath path modul = Path.Combine (path, sprintf "lib%s.dylib" modul.Name)

let makeArgs flags cFile oFile = sprintf "-Wall -std=c99 -arch i386 %s -c %s -o %s" flags cFile oFile

let makeStaticArgs aFile oFiles = sprintf "rcs %s %s" aFile oFiles

let makeDynamicArgs libs oFiles dylibName = sprintf "-arch i386 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 %s %s -o %s " libs oFiles dylibName

let makeClangStartInfo args = ProcessStartInfo ("clang", args)

let makeArStartInfo args = ProcessStartInfo ("ar", args)

let flattenObjectFiles = List.reduce (fun x y -> sprintf "%s %s" x y)

let findAllObjectFiles path = Directory.GetFiles (path, "*.o") |> List.ofArray

let dummyC = ""

let checkProcessError (p: Process) = if p.ExitCode <> 0 then failwith (p.StandardError.ReadToEnd ())

let startClang args = io {
    let pinfo = makeClangStartInfo args

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true

    let p = Process.Start (pinfo)
    p.WaitForExit ()

    checkProcessError p }

let startAr args = io {
    let pinfo = makeArStartInfo args

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true

    let p = Process.Start (pinfo)
    p.WaitForExit ()

    checkProcessError p }

let compileC flags cFile oFile code = io {
    File.WriteAllText (cFile, code)

    let args = makeArgs flags cFile oFile
    do! startClang args

    File.Delete (cFile)
    return oFile }

/// Compiles a dummy c file that contains nothing. This ensures we at least get a dylib.
let compileDummyC outputPath flags = io {
    let cFile = makeDummyCFilePath outputPath
    let oFile = makeDummyOFilePath outputPath

    File.WriteAllText (cFile, dummyC)

    let args = makeArgs flags cFile oFile
    do! startClang args

    File.Delete (cFile)
    return oFile }

let compilationFunctionData modul func =
    makeDllName modul,
    modul.ClangFlagsOsx,
    makeCodeSpec (makeHeaderName modul) func

let compileInlineFunction outputPath modul func definePInvoke = io {
    let dllName, flags, codeSpec = compilationFunctionData modul func

    let cFile = makeCFilePath outputPath codeSpec
    let oFile = makeOFilePath outputPath codeSpec

    let code = generateCode codeSpec

    do! definePInvoke dllName codeSpec
    return! compileC flags cFile oFile code }

let compileExternFunction modul func definePInvoke = io {
    let dllName, _, codeSpec = compilationFunctionData modul func

    do! definePInvoke dllName codeSpec
    return dummyC }

let compileToStaticLibrary aFile oFiles = io {
    let args = makeStaticArgs aFile oFiles
    do! startAr args }

let compileToDynamicLibrary libs oFiles dylibName = io {
    let args = makeDynamicArgs libs oFiles dylibName
    do! startClang args }

let compileFunctions outputPath modul definePInvoke = io {
    let! functions = modul.Functions |> List.map (function
        | Inline x as func ->
            compileInlineFunction outputPath modul func definePInvoke
        | Extern x as func ->
            compileExternFunction modul func definePInvoke)
    let! dummy = compileDummyC outputPath modul.ClangFlagsOsx
    return flattenObjectFiles (dummy :: functions) }

let cleanObjectFiles outputPath = io {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let makeHeaderFile outputPath modul = io {
    let header = makeHFilePath outputPath modul
    let headerCode = generateHeader (makeHeaderName modul) (modul.Includes)
    File.WriteAllText (header, headerCode) }

let compileModule outputPath modul definePInvoke =
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.ClangLibsOsx

    io {
        do! makeHeaderFile outputPath modul
        let! oFiles = compileFunctions outputPath modul definePInvoke
        do! compileToDynamicLibrary libs oFiles dylibName
        return! cleanObjectFiles outputPath }
    |> IO.run
