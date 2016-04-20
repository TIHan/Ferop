module internal Ferop.Compiler

open System
open System.Runtime.InteropServices

open Ferop
open Ferop.Core

[<DllImport("libc")>]
extern int uname (nativeint buf)

//https://github.com/jpobst/Pinta/blob/master/Pinta.Core/Managers/SystemManager.cs#L120
let isRunningOnMac () =
    let mutable buf = IntPtr.Zero
    try
        try
            buf <- Marshal.AllocHGlobal (8192)
            if uname (buf) = 0
            then
                let os = Marshal.PtrToStringAnsi (buf)
                if (os = "Darwin") 
                then true
                else false
            else
                false
        with | _ -> false
    finally
        if (buf <> IntPtr.Zero) then
            Marshal.FreeHGlobal (buf)

let rec makeDllName name = function 
    | Platform.Win -> sprintf "%s.dll" name
    | Platform.Linux -> sprintf "lib%s.so" name
    | Platform.Osx -> sprintf "lib%s.dylib" name
    | Platform.iOS -> "__Internal"
    | _ ->

        match Environment.OSVersion.Platform with
        | x when 
            x = PlatformID.Win32NT ||
            x = PlatformID.Win32S ||
            x = PlatformID.WinCE -> makeDllName name Platform.Win
        | x when x = PlatformID.Unix -> 
            if isRunningOnMac ()
            then makeDllName name Platform.Osx
            else makeDllName name Platform.Linux
        | _ -> failwith "OS not supported."

let rec compileModule path modul = function
    | Platform.Win -> CWin.compileModule path modul
    | Platform.Linux -> CLinux.compileModule path modul
    | Platform.Osx -> COsx.compileModule path modul
    | Platform.iOS -> CiOS.compileModule path modul
    | _ ->

        match Environment.OSVersion.Platform with
        | x when 
            x = PlatformID.Win32NT ||
            x = PlatformID.Win32S ||
            x = PlatformID.WinCE -> compileModule path modul Platform.Win
        | x when x = PlatformID.Unix -> 
            if isRunningOnMac ()
            then compileModule path modul Platform.Osx
            else compileModule path modul Platform.Linux
        | _ -> failwith "OS not supported."