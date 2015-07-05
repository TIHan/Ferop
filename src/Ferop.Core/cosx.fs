[<RequireQualifiedAccess>]
module internal Ferop.COsx

open System.IO
open System.Diagnostics

open Core

let makeOFilePath path modul = Path.Combine (path, sprintf "%s.o" modul.Name)

let makeDynamicLibraryPath path (modul: FeropModule) = Path.Combine (path, sprintf "lib%s.dylib" modul.Name)

let makeArgs flags cFile oFile (modul: FeropModule) =
    let is64bit = (Mono.Cecil.TargetArchitecture.AMD64 = modul.Architecture)

    sprintf """-Wall %s %s -c "%s" -o "%s" """
        (if is64bit then "-arch x86_64" else "-arch i386")
        flags
        cFile
        oFile

let makeStaticArgs aFile oFiles = sprintf "rcs %s %s" aFile oFiles

let makeDynamicArgs libs oFile dylibName (modul: FeropModule) = 
    let is64bit = (Mono.Cecil.TargetArchitecture.AMD64 = modul.Architecture)
    sprintf """%s -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 %s "%s" -o "%s" """
        (if is64bit then "-arch x86_64" else "-arch i386")
        libs oFile dylibName

let makeClangStartInfo args = ProcessStartInfo ("clang", args)

let makeArStartInfo args = ProcessStartInfo ("ar", args)

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
    let flags = modul.ClangFlagsOsx

    let args = makeArgs flags cFile oFile modul
    do! startClang args

    return oFile }

let compileToDynamicLibrary libs oFile dylibName modul = async {
    let args = makeDynamicArgs libs oFile dylibName modul
    do! startClang args }

let cleanObjectFiles outputPath = async {
    return
        findAllObjectFiles outputPath
        |> List.iter (fun x -> File.Delete x) }

let compileModule outputPath modul cgen =
    let dylibName = makeDynamicLibraryPath outputPath modul
    let libs = modul.ClangLibsOsx

    async {
        let! oFile = compileC outputPath modul cgen
        do! compileToDynamicLibrary libs oFile dylibName modul
        return! cleanObjectFiles outputPath }
    |> Async.RunSynchronously
