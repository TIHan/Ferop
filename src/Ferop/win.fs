[<RequireQualifiedAccess>]
module internal Ferop.Win

open System
open System.IO
open System.Diagnostics
open System.Reflection

open Ferop.Code
open Ferop.Core

open FSharp.Control.IO

let bat =
    """call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\vcvars32.bat"
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\cl.exe" %*
"""

let makeDynamicLibraryPath path (modul: Module) = Path.Combine (path, sprintf "%s.dll" modul.Name)

let makeArgs cFile options dllName = sprintf """%s %s /link /DLL /OUT:%s""" cFile options dllName

let makeBatPath path = Path.Combine (path, "msvc.bat")

let makeMsvcStartInfo outputPath args = ProcessStartInfo (makeBatPath outputPath, args)

let writeBat outputPath = io {
    File.WriteAllText (makeBatPath outputPath, bat) }

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

    do! writeBat outputPath
    let args = makeArgs options cFile dllName
    do! startMsvc outputPath args }

let compileModule outputPath modul =
    let cgen = makeCGen outputPath modul

    io {
        do! compileToDynamicLibrary outputPath modul cgen }
    |> IO.run
