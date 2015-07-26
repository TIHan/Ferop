[<RequireQualifiedAccess>]
module internal Ferop.CiOS

open System.IO
open System.Diagnostics

open Core

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

let makeStaticLibraryPath path modul = Path.Combine (path, sprintf "lib%s.a" modul.Name)

let makeArgs flags cFile oFile = sprintf "-Wall -arch i386 %s -c %s -o %s" flags cFile oFile

let makeStaticArgs aFile oFiles = sprintf "rcs %s %s" aFile oFiles

let makeXcRunStartInfo args = ProcessStartInfo ("xcrun", args)

let makeClangStartInfo args = makeXcRunStartInfo ("-sdk iphonesimulator clang " + args)

let makeArStartInfo args = makeXcRunStartInfo ("-sdk iphonesimulator ar " + args)

let flattenObjectFiles = List.reduce (fun x y -> sprintf "%s %s" x y)

let findAllObjectFiles path = Directory.GetFiles (path, "*.o") |> List.ofArray

let startClang args = async {
    let pinfo = makeClangStartInfo args

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true
    pinfo.RedirectStandardOutput <- true

    let p = Process.Start (pinfo)
    p.WaitForExit ()

    checkProcessError "" p }

let startAr args = async {
    let pinfo = makeArStartInfo args

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true
    pinfo.RedirectStandardOutput <- true

    let p = Process.Start (pinfo)
    p.WaitForExit ()

    checkProcessError "" p }

let compileC outputPath modul cgen = async {
    let! _, cFile = writeCGen outputPath modul cgen
    let oFile = makeOFilePath outputPath modul
    let flags = modul.ClangFlagsiOS

    let args = makeArgs flags cFile oFile
    do! startClang args

    return oFile }

let compileToStaticLibrary oFiles libName = async {
    let args = makeStaticArgs libName oFiles
    do! startAr args }

let cleanObjectFiles outputPath = async {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let compileModule outputPath modul cgen =
    let libName = makeStaticLibraryPath outputPath modul

    async {
        let! oFile = compileC outputPath modul cgen
        do! compileToStaticLibrary oFile libName
        return! cleanObjectFiles outputPath }
    |> Async.RunSynchronously
