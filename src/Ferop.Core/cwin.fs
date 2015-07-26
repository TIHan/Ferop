[<RequireQualifiedAccess>]
module internal Ferop.CWin

open System
open System.Globalization
open System.IO
open System.Diagnostics
open Microsoft.Win32

open Core

let bat (is64bit: bool) =
    let registryKeyPath = "SOFTWARE\\Microsoft\\VisualStudio\\SxS\\Vs7"
    let registryKeyPath64bit = "SOFTWARE\\Wow6432Node\\Microsoft\\VisualStudio\\SxS\\Vs7"
    let registryKey = 
        match Registry.LocalMachine.OpenSubKey (registryKeyPath64bit) with
        | null -> 
            match Registry.LocalMachine.OpenSubKey (registryKeyPath) with
            | null -> failwith "Unable to find Visual Studio registry keys."
            | x -> x
        | x -> x

    let key =
        registryKey.GetValueNames ()
        |> Array.choose (fun x -> 
            match Double.TryParse (x, NumberStyles.Number, CultureInfo.InvariantCulture) with
            | (false,_) -> None
            | (true, x) -> 
                Some x)
        |> Array.max

    let vs = registryKey.GetValue (sprintf "%.1f" key) :?> string

    let vcBin = Path.Combine (vs, "VC\\bin")

    let vcBin64bit = Path.Combine (vs, "VC\\bin\\x86_amd64")

    let vcvars32 = Path.Combine (vcBin, "vcvars32.bat")
    let vcvars64 = Path.Combine (vcBin64bit, "vcvarsx86_amd64.bat")
    let cl = "cl.exe"
    let cl32bit = Path.Combine (vcBin, cl)
    let cl64bit = Path.Combine (vcBin64bit, cl)
    let cl, vcvars =
        if is64bit
        then cl64bit, vcvars64
        else cl32bit, vcvars32          
    sprintf
        """call "%s"
call "%s" %%*""" vcvars cl

let makeDynamicLibraryPath path (modul: FeropModule) = Path.Combine (path, sprintf "%s.dll" modul.Name)

let makeArgs options cFile dllName = sprintf """%s "%s" /link /DLL /OUT:"%s" """ options cFile dllName

let makeBatPath path = Path.Combine (path, "msvc.bat")

let makeMsvcStartInfo outputPath args = ProcessStartInfo (makeBatPath outputPath, args)

let findAllObjectFiles path = Directory.GetFiles (path, "*.obj") |> List.ofArray

let writeBat outputPath is64bit = async {
    File.WriteAllText (makeBatPath outputPath, bat is64bit) }

let startMsvc outputPath args = async {
    let pinfo = makeMsvcStartInfo outputPath args

    pinfo.UseShellExecute <- false
    pinfo.RedirectStandardError <- true
    pinfo.RedirectStandardOutput <- true
    pinfo.CreateNoWindow <- true

    let p = Process.Start (pinfo)
    let msg = p.StandardOutput.ReadToEnd ()
    p.WaitForExit ()

    findAllObjectFiles System.Environment.CurrentDirectory
    |> List.iter File.Delete

    checkProcessError msg p }

let compileToDynamicLibrary outputPath modul cgen = async {
    let! _, cFile = writeCGen outputPath modul cgen
    let options = modul.MsvcOptionsWin
    let dllName = makeDynamicLibraryPath outputPath modul

    do! writeBat outputPath (Mono.Cecil.TargetArchitecture.AMD64 = modul.Architecture)
    let args = makeArgs options cFile dllName
    do! startMsvc outputPath args }

let compileModule outputPath modul cgen =
    async {
        do! compileToDynamicLibrary outputPath modul cgen }
    |> Async.RunSynchronously
