[<RequireQualifiedAccess>]
module internal Ferop.Win

open System
open System.IO
open System.Diagnostics
open Microsoft.Win32

open Ferop.Core

open FSharp.Control.IO

let registryKeyPath = "SOFTWARE\\Microsoft\\VisualStudio\\SxS\\Vs7"
let registryKey = Registry.LocalMachine.OpenSubKey (registryKeyPath)
let vs = registryKey.GetValue ("12.0") :?> string

// TODO: Try to find a good solution to handle different VS versions.
let vc12bin = Path.Combine (vs, "VC\\bin")

let vc12bin64bit = Path.Combine (vs, "VC\\bin\\amd64")

let vcvars32 = Path.Combine (vc12bin, "vcvars32.bat")
let vcvars64 = Path.Combine (vc12bin64bit, "vcvars64.bat")
let cl = "cl.exe"
let cl32bit = Path.Combine (vc12bin, cl)
let cl64bit = Path.Combine (vc12bin64bit, cl)

let bat (is64bit: bool) =
    let cl, vcvars =
        if is64bit
        then cl64bit, vcvars64
        else cl32bit, vcvars32          
    sprintf
        """call "%s"
call "%s" %%*""" vcvars cl

let makeDynamicLibraryPath path (modul: Module) = Path.Combine (path, sprintf "%s.dll" modul.Name)

let makeArgs cFile options dllName = sprintf """%s %s /link /DLL /OUT:%s""" cFile options dllName

let makeBatPath path = Path.Combine (path, "msvc.bat")

let makeMsvcStartInfo outputPath args = ProcessStartInfo (makeBatPath outputPath, args)

let writeBat outputPath is64bit = io {
    File.WriteAllText (makeBatPath outputPath, bat is64bit) }

let startMsvc outputPath args = io {
    let pinfo = makeMsvcStartInfo outputPath args

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true
    pinfo.CreateNoWindow <- true

    let p = Process.Start (pinfo)
    p.WaitForExit ()

    checkProcessError p }

let compileToDynamicLibrary outputPath modul cgen = io {
    let! hFile, cFile = writeCGen outputPath modul cgen
    let options = modul.MsvcOptionsWin
    let dllName = makeDynamicLibraryPath outputPath modul

    do! writeBat outputPath modul.IsMsvc64bit
    let args = makeArgs options cFile dllName
    do! startMsvc outputPath args }

let compileModule outputPath modul =
    let cgen = makeCGen outputPath modul

    io {
        do! compileToDynamicLibrary outputPath modul cgen }
    |> IO.run
