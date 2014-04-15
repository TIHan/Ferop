module internal Ferop.Internal

open System
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

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
    modul.Functions |> List.map (definePInvokeMethod tb (makeDllName modul))
    |> ignore

let generateReversePInvokeMethods modul (tb: TypeBuilder) =
    let typ = typeof<Delegate>
    modul.Functions |> List.map (fun x ->
        let del = tb.DefineNestedType (x.Name + "Delegate", typ.Attributes, typ)

        let ctordel = del.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [||])
        let ctordelIL = ctordel.GetILGenerator ()

        ctordelIL.Emit (OpCodes.Nop)
        ctordelIL.Emit (OpCodes.Ret)

        let paramCount = (x.GetParameters ()).Length

        let meth =
            del.DefineMethod (
                "Invoke",
                MethodAttributes.Public,
                x.ReturnType,
                x.GetParameters () |> Array.map (fun x -> x.ParameterType))

        let il = meth.GetILGenerator ()

        match paramCount with
        | 1 ->
            il.Emit (OpCodes.Ldarg_0)
        | 2 ->
            il.Emit (OpCodes.Ldarg_0)
            il.Emit (OpCodes.Ldarg_1)
        | 3 ->
            il.Emit (OpCodes.Ldarg_0)
            il.Emit (OpCodes.Ldarg_1)
            il.Emit (OpCodes.Ldarg_2)
        | 4 ->
            il.Emit (OpCodes.Ldarg_0)
            il.Emit (OpCodes.Ldarg_1)
            il.Emit (OpCodes.Ldarg_2)
            il.Emit (OpCodes.Ldarg_3)
        | _ -> ()

        il.Emit (OpCodes.Call, x)
          
        del.CreateType ())
    
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

        //-------------------------------------------------------------------------
        //-------------------------------------------------------------------------

        let dels = generateReversePInvokeMethods modul tb


        let ctor = tb.DefineTypeInitializer ()
        let il = ctor.GetILGenerator ()

        //dels
        //|> List.iter (fun x -> il.Emit (OpCodes.Newobj, x.GetConstructor ([||]))
        //)
        il.Emit (OpCodes.Nop)
        il.Emit (OpCodes.Ret)


        //-------------------------------------------------------------------------
        //-------------------------------------------------------------------------

        compileModule outputPath modul
        tb.CreateType ())
    |> ignore

    dasm
