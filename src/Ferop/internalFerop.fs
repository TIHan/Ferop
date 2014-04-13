module internal Ferop.Internal

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Reflection

open Ferop.Core
open Ferop.Code
open Ferop.Helpers

let makeDllName modul = 
    let os = Environment.OSVersion

    match os.Platform with
    | x when 
        x = PlatformID.Win32NT ||
        x = PlatformID.Win32S ||
        x = PlatformID.WinCE -> sprintf "%s.dll" modul.Name
    | x when x = PlatformID.Unix -> sprintf "lib%s.dylib" modul.Name
    | _ -> failwith "OS not supported."

let compileModule path modul =
    let os = Environment.OSVersion

    match os.Platform with
    | x when 
        x = PlatformID.Win32NT ||
        x = PlatformID.Win32S ||
        x = PlatformID.WinCE -> Win.compileModule path modul
    | x when x = PlatformID.Unix -> Osx.compileModule path modul
    | _ -> failwith "OS not supported."

let createDynamicAssembly (dllPath: string) dllName =
    AppDomain.CurrentDomain.DefineDynamicAssembly (AssemblyName (dllName), Emit.AssemblyBuilderAccess.RunAndSave, dllPath)

let generatePInvokeMethods modul tb = 
    let meths, delMeths =
        modul.Functions |> List.map (definePInvokeMethod tb (makeDllName modul)),
        modul.Functions |> List.map (definePInvokeMethodDelegate tb (makeDllName modul))

    let ctor = tb.DefineTypeInitializer ()
    let ctorIL = ctor.GetILGenerator ()
 
    ctorIL.Emit (OpCodes.Nop)
 
    meths
    |> List.iter2 (fun x y ->
        ctorIL.Emit (OpCodes.Ldftn, y :> MethodInfo)
        ctorIL.Emit (OpCodes.Call, x :> MethodInfo)) delMeths

    ctorIL.Emit (OpCodes.Ret)
    
let processAssembly dllName (outputPath: string) (dllPath: string) (asm: Assembly) =
    let dasm = createDynamicAssembly dllPath dllName
    let mb = dasm.DefineDynamicModule dllName

    Assembly.modules asm
    |> List.filter (fun x ->
        x.CustomAttributes
        |> Seq.exists (fun x -> x.AttributeType = typeof<FeropAttribute>))
    |> List.map (fun x ->
        let modul = makeModule x
        let tb = mb.DefineType (x.FullName, TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
        generatePInvokeMethods modul tb
        compileModule outputPath modul
        tb.CreateType ())
    |> ignore

    dasm
