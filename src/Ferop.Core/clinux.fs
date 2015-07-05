[<RequireQualifiedAccess>]
module internal Ferop.CLinux

open System.IO
open System.Diagnostics

open Core

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

let makeDynamicLibraryPath path (modul: FeropModule) = Path.Combine (path, sprintf "lib%s.so" modul.Name)

// build-essential; libc6-dev-i386; g++-multilib
let makeArgs flags cFile oFile (modul: FeropModule) =
    let is64bit = (Mono.Cecil.TargetArchitecture.AMD64 = modul.Architecture)

    sprintf """-Wall %s -fPIC %s -c "%s" -o "%s" """
        (if is64bit then "-m64" else "-m32")
        flags
        cFile
        oFile

let makeDynamicArgs libs oFile soName (modul: FeropModule) = 
    let is64bit = (Mono.Cecil.TargetArchitecture.AMD64 = modul.Architecture)
    sprintf """%s "%s" -shared -o "%s" %s""" 
        (if is64bit then "-m64" else "-m32")
        oFile soName libs

let makeGccStartInfo args (modul: FeropModule) = 
    match modul.Language with
    | C -> ProcessStartInfo ("gcc", args)
    | Cpp -> ProcessStartInfo ("g++", args)

let findAllObjectFiles path = Directory.GetFiles (path, "*.o") |> List.ofArray

let startGcc args modul = async {
    let pinfo = makeGccStartInfo args modul

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true
    pinfo.RedirectStandardOutput <- true

    let p = Process.Start (pinfo)
    p.WaitForExit ()

    checkProcessError "" p }

let compileC outputPath modul cgen = async {
    let! _, cFile = writeCGen outputPath modul cgen
    let oFile = makeOFilePath outputPath modul
    let flags = modul.GccFlagsLinux

    let args = makeArgs flags cFile oFile modul
    do! startGcc args modul

    return oFile }

let compileToDynamicLibrary libs oFiles dylibName modul = async {
    let args = makeDynamicArgs libs oFiles dylibName modul
    do! startGcc args modul }

let cleanObjectFiles outputPath = async {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let compileModule outputPath modul cgen =
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.GccLibsLinux

    async {
        let! oFile = compileC outputPath modul cgen
        do! compileToDynamicLibrary libs oFile dylibName modul
        return! cleanObjectFiles outputPath }
    |> Async.RunSynchronously
