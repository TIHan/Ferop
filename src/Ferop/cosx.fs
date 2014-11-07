[<RequireQualifiedAccess>]
module internal FSharp.Interop.FeropInternal.COsx

open System.IO
open System.Diagnostics

open Core

open FSharp.Control.IO

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

let makeDynamicLibraryPath path (modul: FeropModule) = Path.Combine (path, sprintf "lib%s.dylib" modul.Name)

let makeArgs flags cFile oFile (modul: FeropModule) =
    if modul.IsCpp
    then
        sprintf "-Wall -arch i386 %s -c %s -o %s" flags cFile oFile
    else
        sprintf "-Wall -std=c99 -arch i386 %s -c %s -o %s" flags cFile oFile

let makeStaticArgs aFile oFiles = sprintf "rcs %s %s" aFile oFiles

let makeDynamicArgs libs oFiles dylibName = sprintf "-arch i386 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 %s %s -o %s " libs oFiles dylibName

let makeClangStartInfo args = ProcessStartInfo ("clang", args)

let makeArStartInfo args = ProcessStartInfo ("ar", args)

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

    let args = makeArgs flags cFile oFile modul
    do! startClang args

    return oFile }

let compileToDynamicLibrary libs oFiles dylibName = io {
    let args = makeDynamicArgs libs oFiles dylibName
    do! startClang args }

let cleanObjectFiles outputPath = io {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let compileModule outputPath modul cgen =
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.ClangLibsOsx

    io {
        let! oFile = compileC outputPath modul cgen
        do! compileToDynamicLibrary libs oFile dylibName
        return! cleanObjectFiles outputPath }
    |> IO.run
