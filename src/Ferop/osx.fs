[<RequireQualifiedAccess>]
module internal Ferop.Osx

open System
open System.IO
open System.Diagnostics

open Ferop.Code
open Ferop.Core
open Ferop.Helpers
open Ferop.CConversion
open Ferop.CGen

open FSharp.Control.IO

let makeHeaderName modul = modul.Name

let makeHeaderInclude name = sprintf "#include \"%s.h\" \n" name

let makeHeaderIncludes (modul: Module) = modul.Includes

let makeHFilePath path modul = Path.Combine (path, sprintf "%s.h" modul.Name)

let makeCFilePath path modul = Path.Combine (path, sprintf "%s.c" modul.Name)

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

let makeDummyCFilePath path = Path.Combine (path, "_ferop_dummy_.c")

let makeDummyOFilePath path = Path.Combine (path, "_ferop_dummy_.o")

let makeStaticLibraryPath path modul = Path.Combine (path, sprintf "lib%s.a" modul.Name)

let makeDynamicLibraryPath path (modul: Module) = Path.Combine (path, sprintf "lib%s.dylib" modul.Name)

let makeArgs flags cFile oFile = sprintf "-Wall -std=c99 -arch i386 %s -c %s -o %s" flags cFile oFile

let makeStaticArgs aFile oFiles = sprintf "rcs %s %s" aFile oFiles

let makeDynamicArgs libs oFiles dylibName = sprintf "-arch i386 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 %s %s -o %s " libs oFiles dylibName

let makeClangStartInfo args = ProcessStartInfo ("clang", args)

let makeArStartInfo args = ProcessStartInfo ("ar", args)

let flattenObjectFiles = List.reduce (fun x y -> sprintf "%s %s" x y)

let findAllObjectFiles path = Directory.GetFiles (path, "*.o") |> List.ofArray

let dummyC = ""

let checkProcessError (p: Process) = if p.ExitCode <> 0 then failwith (p.StandardError.ReadToEnd ())

let generateC includes body =
    sprintf """%s
%s
"""
        includes body

let makeC outputPath modul =
    let tast = makeTypedAst modul.Functions
    let gen = generate tast
    generateC (makeHeaderInclude (makeHeaderName modul)) gen

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

let compileC outputPath modul code = io {
    let cFile = makeCFilePath outputPath modul
    let oFile = makeOFilePath outputPath modul
    let flags = modul.ClangFlagsOsx

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

let compileToStaticLibrary aFile oFiles = io {
    let args = makeStaticArgs aFile oFiles
    do! startAr args }

let compileToDynamicLibrary libs oFiles dylibName = io {
    let args = makeDynamicArgs libs oFiles dylibName
    do! startClang args }

let cleanObjectFiles outputPath = io {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let makeHeaderFile outputPath modul = io {
    let header = makeHFilePath outputPath modul
    let headerGen = generateMainHeader (makeHeaderName modul) modul.Includes
    File.WriteAllText (header, headerGen)
    return header }

let compileModule outputPath modul =
    let code = makeC outputPath modul
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.ClangLibsOsx

    io {
        let! header = makeHeaderFile outputPath modul
        let! oFile = compileC outputPath modul code
        do! compileToDynamicLibrary libs oFile dylibName
        File.Delete (header)
        return! cleanObjectFiles outputPath }
    |> IO.run
