[<RequireQualifiedAccess>]
module internal FSharp.Interop.FeropInternal.CLinux

open System.IO
open System.Diagnostics

open Core

open FSharp.Control.IO

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

let makeDynamicLibraryPath path (modul: Module) = Path.Combine (path, sprintf "lib%s.so" modul.Name)

// build-essential; libc6-dev-i386; g++-multilib
let makeArgs flags cFile oFile (modul: Module) =
    if modul.IsCpp
    then
        sprintf "-Wall -m64 %s -c %s -o %s" flags cFile oFile
    else
        sprintf "-Wall -std=c99 -m64 %s -c %s -o %s" flags cFile oFile

let makeDynamicArgs libs oFile soName = sprintf "-m64 -fPIC %s %s -shared -o %s" libs oFile soName

let makeGccStartInfo args (modul: Module) = 
    if modul.IsCpp
    then ProcessStartInfo ("g++", args)
    else ProcessStartInfo ("gcc", args)

let findAllObjectFiles path = Directory.GetFiles (path, "*.o") |> List.ofArray

let startGcc args modul = io {
    let pinfo = makeGccStartInfo args modul

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true

    let p = Process.Start (pinfo)
    p.WaitForExit ()

    checkProcessError p }

let compileC outputPath modul cgen = io {
    let! _, cFile = writeCGen outputPath modul cgen
    let oFile = makeOFilePath outputPath modul
    let flags = modul.GccFlagsLinux

    let args = makeArgs flags cFile oFile modul
    do! startGcc args modul

    return oFile }

let compileToDynamicLibrary libs oFiles dylibName modul = io {
    let args = makeDynamicArgs libs oFiles dylibName
    do! startGcc args modul }

let cleanObjectFiles outputPath = io {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let compileModule outputPath modul =
    let cgen = makeCGen modul
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.GccLibsLinux

    io {
        let! oFile = compileC outputPath modul cgen
        do! compileToDynamicLibrary libs oFile dylibName modul
        return! cleanObjectFiles outputPath }
    |> IO.run
