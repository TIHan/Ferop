module internal Ferop.Internal

open System
open System.Security
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

let generateReversePInvokeDelegates modul (tb: ModuleBuilder) =
    let typ = typeof<MulticastDelegate>
    modul.ExportedFunctions |> List.map (fun x ->
        let del = tb.DefineType (x.Name + "Delegate", TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable, typ)

        let ctordel = del.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [|typeof<obj>; typeof<nativeint>|])
//        let ctordel = del.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [|typeof<Type>;typeof<string>|])
        ctordel.SetImplementationFlags (ctordel.GetMethodImplementationFlags () ||| MethodImplAttributes.Runtime)

        let meth =
            del.DefineMethod (
                "Invoke",
                MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig,
                x.ReturnType,
                x.GetParameters () |> Array.map (fun x -> x.ParameterType))

        meth.SetImplementationFlags (meth.GetMethodImplementationFlags () ||| MethodImplAttributes.Runtime)
        let attributeType = typeof<UnmanagedFunctionPointerAttribute>
        let attributeConstructorInfo = attributeType.GetConstructor ([|typeof<CallingConvention>|])
        let attributeBuilder = CustomAttributeBuilder (attributeConstructorInfo, [|CallingConvention.Cdecl|])
        del.SetCustomAttribute (attributeBuilder)     
        del.CreateType ())

let generateReversePInvokeMethods modul (tb: TypeBuilder) =
    let dllName = makeDllName modul
    modul.ExportedFunctions
    |> List.map (fun func ->
        let meth = 
            tb.DefinePInvokeMethod (
                sprintf "ferop_set_fs_%s_%s" func.DeclaringType.Name func.Name,
                dllName,
                sprintf "ferop_set_fs_%s_%s" func.DeclaringType.Name func.Name,
                MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PinvokeImpl,
                CallingConventions.Standard,
                typeof<Void>,
                [|typeof<Delegate>|],
                CallingConvention.Cdecl,
                CharSet.Ansi)

        meth.SetImplementationFlags (meth.GetMethodImplementationFlags () ||| MethodImplAttributes.PreserveSig)
        let attributeType = typeof<SuppressUnmanagedCodeSecurityAttribute>
        let attributeConstructorInfo = attributeType.GetConstructor ([||])
        let attributeBuilder = CustomAttributeBuilder (attributeConstructorInfo, [||])
        meth.SetCustomAttribute (attributeBuilder)
        meth)

let feropModules asm =
    Assembly.modules asm
    |> List.filter (fun x ->
        x.CustomAttributes
        |> Seq.exists (fun x -> x.AttributeType = typeof<FeropAttribute>))
    
let processAssembly dllName (outputPath: string) (dllPath: string) (asm: Assembly) =
    let dasm = createDynamicAssembly dllPath dllName
    let mb = dasm.DefineDynamicModule dllName

    feropModules asm
    |> List.map (fun x ->
        let modul = makeModule x
        let tb = mb.DefineType (x.FullName, TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
        generatePInvokeMethods modul tb

        //-------------------------------------------------------------------------
        //-------------------------------------------------------------------------

        let dels = generateReversePInvokeDelegates modul mb
        let delMeths = generateReversePInvokeMethods modul tb

        let ctor = tb.DefineTypeInitializer ()
        let il = ctor.GetILGenerator ()

        let del = dels.[0]
        let delMeth = delMeths.[0]

        //il.Emit (OpCodes.Ldtoken, tb)
        //il.Emit (OpCodes.Call, typeof<Type>.GetMethod ("GetTypeFromHandle", [|typeof<RuntimeTypeHandle>|]))
        //il.Emit (OpCodes.Ldstr, delMeth.Name)
        //il.Emit (OpCodes.Newobj, del.GetConstructor ([|typeof<Type>;typeof<string>|]))

        il.Emit (OpCodes.Ldnull)
        il.Emit (OpCodes.Ldftn, modul.ExportedFunctions.[0])
        il.Emit (OpCodes.Newobj, del.GetConstructor ([|typeof<obj>;typeof<nativeint>|]))
        il.Emit (OpCodes.Call, delMeth)
       // il.Emit (OpCodes.Pop)
        il.Emit (OpCodes.Ret)



        //-------------------------------------------------------------------------
        //-------------------------------------------------------------------------

        compileModule outputPath modul
        tb.CreateType ())
    |> ignore

    dasm
