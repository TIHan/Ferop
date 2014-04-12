[<RequireQualifiedAccess>]
module internal Ferop.Osx

open System
open System.IO
open System.Diagnostics

open Ferop.Code
open Ferop.Core
open Ferop.Helpers

open FSharp.Control.IO

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

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

let compileC outputPath modul cgen = io {
    let! hFile, cFile = writeCGen outputPath modul cgen
    let oFile = makeOFilePath outputPath modul
    let flags = modul.ClangFlagsOsx

    let args = makeArgs flags cFile oFile
    do! startClang args

#if DEBUG
#else
    File.Delete hFile
    File.Delete cFile
#endif
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

let compileModule outputPath modul =
    let cgen = makeCGen outputPath modul
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.ClangLibsOsx

    io {
        let! oFile = compileC outputPath modul cgen
        do! compileToDynamicLibrary libs oFile dylibName
        return! cleanObjectFiles outputPath }
    |> IO.run
