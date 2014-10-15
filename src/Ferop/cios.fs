[<RequireQualifiedAccess>]
module internal FSharp.Interop.FeropInternal.CiOS

open System.IO
open System.Diagnostics

open Core

open FSharp.Control.IO

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

let makeDummyOFilePath path = Path.Combine (path, "_ferop_dummy_.o")

let makeStaticLibraryPath path modul = Path.Combine (path, sprintf "lib%s.a" modul.Name)

let makeArgs flags cFile oFile = sprintf "-Wall -std=c99 -arch i386 %s -c %s -o %s" flags cFile oFile

let makeStaticArgs aFile oFiles = sprintf "rcs %s %s" aFile oFiles

let makeXcRunStartInfo args = ProcessStartInfo ("xcrun", args)

let makeClangStartInfo args = makeXcRunStartInfo ("-sdk iphonesimulator clang " + args)

let makeArStartInfo args = makeXcRunStartInfo ("-sdk iphonesimulator ar " + args)

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
    let! _, cFile = writeCGen outputPath modul cgen
    let oFile = makeOFilePath outputPath modul
    let flags = modul.ClangFlagsOsx

    let args = makeArgs flags cFile oFile
    do! startClang args

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

let compileToStaticLibrary oFiles libName = io {
    let args = makeStaticArgs libName oFiles
    do! startAr args }

let cleanObjectFiles outputPath = io {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let compileModule outputPath modul =
    let cgen = makeCGen modul
    let libName = makeStaticLibraryPath outputPath modul

    io {
        let! oFile = compileC outputPath modul cgen
        do! compileToStaticLibrary oFile libName
        return! cleanObjectFiles outputPath }
    |> IO.run
