namespace Ferop

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Security
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open Mono.Cecil

open FSharp.Interop.Ferop
open FSharp.Interop.FeropCompiler
open FSharp.Interop.FeropInternal
open FSharp.Interop.FeropInternal.Core

[<Serializable>]
type Proxy () =
    inherit MarshalByRefObject ()

    let hasAttribute (typ: Type) (typDef: TypeDefinition) =
        typDef.CustomAttributes
        |> Seq.exists (fun x ->
            x.AttributeType.FullName.Contains(typ.Name))

    let methodHasAttribute (typ: Type) (methDef: MethodDefinition) =
        methDef.CustomAttributes
        |> Seq.exists (fun x ->
            x.AttributeType.FullName.Contains(typ.Name))

    let classes (asm: Assembly) =
        asm.GetTypes ()
        |> Array.filter (fun x -> x.IsClass && x.IsDefined (typeof<FeropAttribute>))
        |> List.ofArray

    member this.Execute (assemblyPath: string, references: string, targetDirectory: string) : unit = 
        let asmDef = AssemblyDefinition.ReadAssembly (assemblyPath)

        asmDef.Modules
        |> Seq.iter (fun m ->
            let voidType = m.Import(typeof<Void>)
            let objType = m.Import(typeof<obj>)
            let nativeintType = m.Import(typeof<nativeint>)
            let delType = m.Import(typeof<MulticastDelegate>)
            let compilerGeneratedAttrCtor = m.Import(typeof<CompilerGeneratedAttribute>.GetConstructor(Array.empty))
            let unmanagedFnPtrCtor = m.Import(typeof<UnmanagedFunctionPointerAttribute>.GetConstructor([|typeof<CallingConvention>|]))
            let callingConvType = m.Import(typeof<CallingConvention>)
            let dllimportAttrTypeCtor = m.Import(typeof<DllImportAttribute>.GetConstructor([|typeof<string>|]))
            let stringType = m.Import(typeof<string>)
            let unSecuAttrCtor = m.Import(typeof<SuppressUnmanagedCodeSecurityAttribute>.GetConstructor(Array.empty))

            m.GetTypes ()
            |> Array.ofSeq
            |> Array.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
            |> Array.iter (fun x -> 
                let mref = ModuleReference (FSharp.Interop.FeropCompiler.C.makeDllName x.Name Platform.Auto)

                x.Methods
                |> Array.ofSeq
                |> Array.iter (fun meth ->
                    if methodHasAttribute typeof<ExportAttribute> meth then
                        let del = TypeDefinition (meth.DeclaringType.Namespace, meth.Name + "Delegate", TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable, delType)

                        let ctordel = MethodDefinition (".ctor", MethodAttributes.Public ||| MethodAttributes.CompilerControlled ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, voidType)
                        ctordel.Parameters.Add (ParameterDefinition ("'object'", ParameterAttributes.None, objType))
                        ctordel.Parameters.Add (ParameterDefinition ("'method'", ParameterAttributes.None, nativeintType))
                        ctordel.ImplAttributes <- ctordel.ImplAttributes ||| MethodImplAttributes.Runtime

                        del.Methods.Add (ctordel)

                        let delmeth = MethodDefinition ("Invoke", MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.HideBySig, meth.ReturnType)
                        delmeth.ImplAttributes <- delmeth.ImplAttributes ||| MethodImplAttributes.Runtime

                        let customAttr = CustomAttribute (compilerGeneratedAttrCtor)
                        del.CustomAttributes.Add (customAttr)

                        let customAttr = CustomAttribute (unmanagedFnPtrCtor)
                        customAttr.ConstructorArguments.Add (CustomAttributeArgument (callingConvType, CallingConvention.Cdecl))
                        del.CustomAttributes.Add (customAttr)

                        meth.Parameters
                        |> Seq.iter delmeth.Parameters.Add

                        del.Methods.Add (delmeth)

                        m.Types.Add del

                        // ******

                        let meth = 
                            MethodDefinition (
                                sprintf "_ferop_set_%s" meth.Name,
                                MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PInvokeImpl ||| MethodAttributes.HideBySig,
                                voidType)

                        meth.IsPInvokeImpl <- true
                        meth.IsPreserveSig <- true
                        meth.PInvokeInfo <-
                            PInvokeInfo (PInvokeAttributes.CallConvCdecl ||| PInvokeAttributes.CharSetAnsi, sprintf "%s__%s" x.Name meth.Name, mref)
                        meth.Parameters.Add (ParameterDefinition ("ptr", ParameterAttributes.None, del))

                        let customAttr = CustomAttribute (unSecuAttrCtor)
                        meth.CustomAttributes.Add (customAttr)

                        x.Methods.Add meth
                    else
                        ()
                )
            )
            m.Write (assemblyPath + "tmp")
        )


        let load (x: string) = Assembly.LoadFrom (x)
        let asm = load (assemblyPath + "tmp")

        references.Split(';')
        |> Array.iter (fun x -> load x |> ignore)

        let asmDef = AssemblyDefinition.ReadAssembly (assemblyPath + "tmp")  

        asmDef.Modules
        |> Seq.iter (fun m ->
            let unSecuAttrCtor = m.Import(typeof<SuppressUnmanagedCodeSecurityAttribute>.GetConstructor(Array.empty))

            m.GetTypes ()
            |> Seq.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
            |> Seq.iter (fun x -> 
                let mref = ModuleReference (FSharp.Interop.FeropCompiler.C.makeDllName x.Name Platform.Auto)

                x.CustomAttributes.Remove (
                    x.CustomAttributes
                    |> Seq.find (fun x -> x.AttributeType.Name.Contains ("Ferop"))) |> ignore

                x.Methods
                |> Array.ofSeq
                |> Array.iter (fun meth ->
                    if methodHasAttribute typeof<ExportAttribute> meth then
                        ()
                    else
                        meth.Attributes <- MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PInvokeImpl ||| MethodAttributes.HideBySig
                        meth.IsPInvokeImpl <- true
                        meth.IsPreserveSig <- true
                        meth.PInvokeInfo <-
                            PInvokeInfo (PInvokeAttributes.CallConvCdecl ||| PInvokeAttributes.CharSetAnsi, sprintf "%s_%s" x.Name meth.Name, mref)

                        let customAttr = CustomAttribute (unSecuAttrCtor)
                        meth.CustomAttributes.Add (customAttr)
                )
                asmDef.MainModule.ModuleReferences.Add mref
            )
            m.Write (assemblyPath)
        )

        classes asm
        |> List.iter (fun m ->
            let modul = makeModule m
            let cgen = makeCGen modul
            FSharp.Interop.FeropCompiler.C.compileModule targetDirectory modul Platform.Auto cgen)

[<Serializable>]
type public WeavingTask () =
    inherit Task ()

    [<Required>]
    member val AssemblyPath : string = "" with get, set

    [<Required>]
    member val ProjectDirectory : string = "" with get, set

    [<Required>]
    member val TargetDirectory : string = "" with get, set

    member val References : string = "" with get, set

    override this.Execute () : bool =  
        let currentAsm = Assembly.GetExecutingAssembly ()

        let domaininfo = AppDomainSetup ()
        domaininfo.ApplicationBase <- System.Environment.CurrentDirectory
        let evidence = AppDomain.CurrentDomain.Evidence;
        let appDomain = AppDomain.CreateDomain ("Ferop", evidence, domaininfo)

        this.Log.LogMessage ("AppDomain Created.")

        let proxy : Proxy = appDomain.CreateInstanceFromAndUnwrap (currentAsm.Location, typeof<Proxy>.FullName) :?> Proxy
        this.Log.LogMessage ("Executing...")
        proxy.Execute (this.AssemblyPath, this.References, this.TargetDirectory)
        this.Log.LogMessage ("Done.")
 
        AppDomain.Unload appDomain
        this.Log.LogMessage ("AppDomain Unloaded.")
        true
