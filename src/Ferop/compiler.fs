module FSharp.Interop.FeropCompiler

open System
open System.IO
open System.Security
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

open FSharp.Interop.Ferop
open FSharp.Interop.FeropInternal
open FSharp.Interop.FeropInternal.Core

[<RequireQualifiedAccess>]
module Ferop =
    let rec makeDllName modul = function 
        | Platform.Win -> sprintf "%s.dll" modul.Name
        | Platform.Linux -> failwith "Linux not supported."
        | Platform.Osx -> sprintf "lib%s.dylib" modul.Name
        | Platform.AppleiOS -> "__Internal"
        | _ ->

        match Environment.OSVersion.Platform with
        | x when 
            x = PlatformID.Win32NT ||
            x = PlatformID.Win32S ||
            x = PlatformID.WinCE -> makeDllName modul Platform.Win
        | x when x = PlatformID.Unix -> makeDllName modul Platform.Osx
        | _ -> failwith "OS not supported."

    let rec compileModule path modul = function
        | Platform.Win -> Win.compileModule path modul
        | Platform.Linux -> failwith "Linux not supported."
        | Platform.Osx -> Osx.compileModule path modul
        | Platform.AppleiOS -> iOS.compileModule path modul
        | _ ->

        match Environment.OSVersion.Platform with
        | x when 
            x = PlatformID.Win32NT ||
            x = PlatformID.Win32S ||
            x = PlatformID.WinCE -> compileModule path modul Platform.Win
        | x when x = PlatformID.Unix -> compileModule path modul Platform.Osx
        | _ -> failwith "OS not supported."

    let createDynamicAssembly (dllPath: string) dllName =
        AppDomain.CurrentDomain.DefineDynamicAssembly (AssemblyName (dllName), Emit.AssemblyBuilderAccess.RunAndSave, dllPath)

    let generatePInvokeMethods modul platform tb = 
        modul.Functions |> List.map (definePInvokeMethod tb (makeDllName modul platform))
        |> ignore

    let generateReversePInvokeDelegates modul (tb: ModuleBuilder) =
        let typ = typeof<MulticastDelegate>
        modul.ExportedFunctions |> List.map (fun x ->
            let del = tb.DefineType (x.Name + "Delegate", TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable, typ)

            let ctordel = del.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [|typeof<obj>; typeof<nativeint>|])
            ctordel.SetImplementationFlags (ctordel.GetMethodImplementationFlags () ||| MethodImplAttributes.Runtime)

            let meth =
                del.DefineMethod (
                    "Invoke",
                    MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig,
                    x.ReturnType,
                    x.GetParameters () |> Array.map (fun x -> x.ParameterType))

            x.GetParameters ()
            |> Array.iteri (fun i x ->
                meth.DefineParameter (i + 1, ParameterAttributes.None, x.Name) |> ignore)

            meth.SetImplementationFlags (meth.GetMethodImplementationFlags () ||| MethodImplAttributes.Runtime)
            let attributeType = typeof<UnmanagedFunctionPointerAttribute>
            let attributeConstructorInfo = attributeType.GetConstructor ([|typeof<CallingConvention>|])
            let attributeBuilder = CustomAttributeBuilder (attributeConstructorInfo, [|CallingConvention.Cdecl|])
            del.SetCustomAttribute (attributeBuilder)     
            del.CreateType ())

    let generateReversePInvokeMethods modul dels platform (tb: TypeBuilder) =
        let dllName = makeDllName modul platform
        modul.ExportedFunctions
        |> List.map2 (fun del func ->
            let meth = 
                tb.DefinePInvokeMethod (
                    sprintf "ferop_set_%s" func.Name,
                    dllName,
                    sprintf "%s_ferop_set_%s" func.DeclaringType.Name func.Name,
                    MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PinvokeImpl,
                    CallingConventions.Standard,
                    typeof<Void>,
                    [|del|],
                    CallingConvention.Cdecl,
                    CharSet.Ansi)

            meth.DefineParameter (1, ParameterAttributes.None, "ptr") |> ignore

            meth.SetImplementationFlags (meth.GetMethodImplementationFlags () ||| MethodImplAttributes.PreserveSig)
            let attributeType = typeof<SuppressUnmanagedCodeSecurityAttribute>
            let attributeConstructorInfo = attributeType.GetConstructor ([||])
            let attributeBuilder = CustomAttributeBuilder (attributeConstructorInfo, [||])
            meth.SetCustomAttribute (attributeBuilder)
            meth :> MethodInfo) dels

    let feropClasses (asm: Assembly) =
        asm.GetTypes ()
        |> Array.filter (fun x ->x.IsClass)
        |> Array.filter (fun x ->
            x.CustomAttributes
            |> Seq.exists (fun x -> x.AttributeType = typeof<ReflectedDefinitionAttribute>))
        |> List.ofArray
    
    let processAssembly dllName (outputPath: string) (dllPath: string) (canCompileModule: bool) (platform: Platform) (asm: Assembly) =
        let dasm = createDynamicAssembly dllPath dllName
        let mb = dasm.DefineDynamicModule dllName

        feropClasses asm
        |> List.map (fun x ->
            let modul = makeModule x
            let tb = mb.DefineType (x.FullName, TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
            generatePInvokeMethods modul platform tb

            //-------------------------------------------------------------------------
            //-------------------------------------------------------------------------

            let dels = generateReversePInvokeDelegates modul mb
            let delMeths = generateReversePInvokeMethods modul dels platform tb

            let ctor = tb.DefineTypeInitializer ()
            let il = ctor.GetILGenerator ()

            dels
            |> List.iteri2 (fun i delMeth del ->
                let func = modul.ExportedFunctions.[i]
                let fieldName = "_" + func.Name
                let field = tb.DefineField ("_" + func.Name, del, FieldAttributes.Static ||| FieldAttributes.Public)

                il.Emit (OpCodes.Ldnull)
                il.Emit (OpCodes.Ldftn, func)
                il.Emit (OpCodes.Newobj, del.GetConstructor ([|typeof<obj>;typeof<nativeint>|]))
                il.Emit (OpCodes.Stsfld, field)
                il.Emit (OpCodes.Ldsfld, field)
                il.Emit (OpCodes.Call, delMeth :> MethodInfo)) delMeths

            il.Emit (OpCodes.Ret)

            tb.CreateType () |> ignore
            //-------------------------------------------------------------------------
            //-------------------------------------------------------------------------
            let modul' = 
                { modul with 
                    Functions = modul.Functions @ (delMeths |> List.map (fun x -> tb.GetMethod (x.Name))) }

            if canCompileModule then compileModule outputPath modul' platform

            ()) |> ignore

        dasm

    let compileDynamic name outputPath dllPath canCompileModule platform asm =
        let dllName = Path.GetFileName (Path.ChangeExtension (name, ".dll"))
        processAssembly dllName outputPath dllPath canCompileModule platform asm
    
    let compile name outputPath dllPath canCompileModule platform asm =
        let asm = compileDynamic name outputPath dllPath canCompileModule platform asm
        let asmName = asm.GetName ()
        asm.Save (asmName.Name)
        Path.Combine (dllPath, asmName.Name)