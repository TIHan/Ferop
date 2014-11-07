namespace Ferop

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open Mono.Cecil

open FSharp.Interop.Ferop
open FSharp.Interop.FeropCompiler
open FSharp.Interop.FeropInternal
open FSharp.Interop.FeropInternal.Core

type public WeavingTask () =
    inherit Task ()

    let hasAttribute (typ: Type) (typDef: TypeDefinition) =
        typDef.CustomAttributes
        |> Seq.exists (fun x ->
            x.AttributeType.FullName.Contains(typ.Name))

    let methodHasAttribute (typ: Type) (methDef: MethodDefinition) =
        methDef.CustomAttributes
        |> Seq.exists (fun x ->
            x.AttributeType.FullName.Contains(typ.Name))

    let feropClasses (asm: Assembly) =
        asm.GetTypes ()
        |> Array.filter (fun x ->x.IsClass)
        |> Array.filter (fun x ->
            x.CustomAttributes
            |> Seq.exists (fun x -> x.AttributeType = typeof<FeropAttribute>))
        |> List.ofArray

    let rec makeDllName name = function 
        | Platform.Win -> sprintf "%s.dll" name
        | Platform.Linux -> sprintf "lib%s.so" name
        | Platform.Osx -> sprintf "lib%s.dylib" name
        //| Platform.AppleiOS -> "__Internal"
        | _ ->

        match Environment.OSVersion.Platform with
        | x when 
            x = PlatformID.Win32NT ||
            x = PlatformID.Win32S ||
            x = PlatformID.WinCE -> makeDllName name Platform.Win
        | x when x = PlatformID.Unix -> 
            if C.isRunningOnMac ()
            then makeDllName name Platform.Osx
            else makeDllName name Platform.Linux
        | _ -> failwith "OS not supported."

    [<Required>]
    member val AssemblyPath : string = "" with get, set

    [<Required>]
    member val ProjectDirectory : string = "" with get, set

    [<Required>]
    member val TargetDirectory : string = "" with get, set

    override this.Execute () : bool =  
//        let asmDef = AssemblyDefinition.ReadAssembly (this.AssemblyPath)
//
//        asmDef.Modules
//        |> Seq.iter (fun m ->
//            m.GetTypes ()
//            |> Array.ofSeq
//            |> Array.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
//            |> Array.iter (fun x -> 
//                x.Methods
//                |> Array.ofSeq
//                |> Array.iter (fun meth ->
//                    if methodHasAttribute typeof<ExportAttribute> meth then
//                        let voidType = m.Import(typeof<Void>)
//                        let objType = m.Import(typeof<obj>)
//                        let nativeintType = m.Import(typeof<nativeint>)
//                        let delType = m.Import(typeof<MulticastDelegate>)
//                        let compilerGeneratedAttrCtor = m.Import(typeof<CompilerGeneratedAttribute>.GetConstructor(Array.empty))
//                        let unmanagedFnPtrCtor = m.Import(typeof<UnmanagedFunctionPointerAttribute>.GetConstructor([|typeof<CallingConvention>|]))
//                        let callingConvType = m.Import(typeof<CallingConvention>)
//                        let dllimportAttrTypeCtor = m.Import(typeof<DllImportAttribute>.GetConstructor([|typeof<string>|]))
//                        let stringType = m.Import(typeof<string>)
//
//                        let del = TypeDefinition (meth.DeclaringType.Namespace, meth.Name + "Delegate", TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable, delType)
//
//                        let ctordel = MethodDefinition (".ctor", MethodAttributes.Public ||| MethodAttributes.CompilerControlled ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, voidType)
//                        ctordel.Parameters.Add (ParameterDefinition ("'object'", ParameterAttributes.None, objType))
//                        ctordel.Parameters.Add (ParameterDefinition ("'method'", ParameterAttributes.None, nativeintType))
//                        ctordel.ImplAttributes <- ctordel.ImplAttributes ||| MethodImplAttributes.Runtime
//
//                        del.Methods.Add (ctordel)
//
//                        let delmeth = MethodDefinition ("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig, meth.ReturnType)
//                        delmeth.ImplAttributes <- delmeth.ImplAttributes ||| MethodImplAttributes.Runtime
//
//                        let customAttr = CustomAttribute (compilerGeneratedAttrCtor)
//                        del.CustomAttributes.Add (customAttr)
//
//                        let customAttr = CustomAttribute (unmanagedFnPtrCtor)
//                        customAttr.ConstructorArguments.Add (CustomAttributeArgument (callingConvType, CallingConvention.Cdecl))
//                        del.CustomAttributes.Add (customAttr)
//
//                        meth.Parameters
//                        |> Seq.iter delmeth.Parameters.Add
//
//                        del.Methods.Add (delmeth)
//
//                        m.Types.Add del
//
//                        // ******
//
//                        let meth = 
//                            MethodDefinition (
//                                sprintf "_ferop_set_%s" meth.Name,
//                                MethodAttributes.Public ||| MethodAttributes.Static,
//                                voidType
//                            )
//                        meth.IsPInvokeImpl <- true
//                        meth.IsPreserveSig <- true
//                        meth.Parameters.Add (ParameterDefinition ("ptr", ParameterAttributes.None, del))
//
//                        let customAttr = CustomAttribute (dllimportAttrTypeCtor)
//                        customAttr.ConstructorArguments.Add (CustomAttributeArgument (stringType, makeDllName x.Name Platform.Auto))
//                        meth.CustomAttributes.Add (customAttr)
//
//                        x.Methods.Add meth
//                    else
//                        ()
//                )
//            )
//            m.Write (this.AssemblyPath)
//        )
//
        let asmBytes = System.IO.File.ReadAllBytes (this.AssemblyPath)
        let asm = Assembly.Load asmBytes

//        let asmDef = AssemblyDefinition.ReadAssembly (this.AssemblyPath)  
//
//        asmDef.Modules
//        |> Seq.iter (fun m ->
//            let mref = ModuleReference (makeDllName "Tests" Platform.Auto)
//            m.GetTypes ()
//            |> Seq.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
//            |> Seq.iter (fun x -> 
//                x.Methods
//                |> Array.ofSeq
//                |> Array.iter (fun meth ->
//                    if methodHasAttribute typeof<ExportAttribute> meth then
//                        ()
//                    else
//                        meth.Attributes <- MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PInvokeImpl
//                        meth.IsPInvokeImpl <- true
//                        meth.PInvokeInfo <-
//                            PInvokeInfo (PInvokeAttributes.CallConvCdecl ||| PInvokeAttributes.CharSetAnsi, sprintf "%s_%s" x.Name meth.Name, mref)
//                )
//            )
//            asmDef.MainModule.ModuleReferences.Add mref
//            m.Write (this.AssemblyPath)
//        )

        feropClasses asm
        |> List.iter (fun m ->
            let modul = makeModule m
            C.compileModule this.TargetDirectory modul Platform.Auto
        )
        true