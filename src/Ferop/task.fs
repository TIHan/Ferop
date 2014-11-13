﻿namespace Ferop

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open System.Security
open System.Reflection
open Microsoft.Build.Framework
open Microsoft.Build.Utilities
open Mono.Cecil

open Ferop
open Ferop.Compiler
open Ferop.Core

[<Serializable>]
type internal Proxy () =
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
        |> Array.filter (fun x -> x.IsClass)
        |> Array.filter (fun x ->
            x.GetCustomAttributesData ()
            |> Seq.exists (fun x -> x.AttributeType.FullName = typeof<FeropAttribute>.FullName))
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
            let unSecuAttrCtor = m.Import(typeof<SuppressUnmanagedCodeSecurityAttribute>.GetConstructor(Array.empty))
            let importAttrCtor = m.Import(typeof<ImportAttribute>.GetConstructor(Array.empty))

            m.GetTypes ()
            |> Array.ofSeq
            |> Array.filter (fun x -> x.HasMethods && hasAttribute typeof<FeropAttribute> x)
            |> Array.iter (fun x ->
                let name = makeDllName x.Name Platform.Auto

                let mref =
                    match
                        asmDef.MainModule.ModuleReferences
                        |> Seq.tryFind (fun x -> x.Name = name) with
                    | Some x -> x
                    | None ->
                        let mref = ModuleReference name
                        asmDef.MainModule.ModuleReferences.Add mref
                        mref

                let exportedMeths = ResizeArray<MethodDefinition> ()
                let exportedDels = ResizeArray<TypeDefinition> ()
                let exportedSetMeths = ResizeArray<MethodDefinition> ()

                x.Methods
                |> Array.ofSeq
                |> Array.filter (fun x -> not x.IsConstructor)
                |> Array.filter (fun x -> methodHasAttribute typeof<ImportAttribute> x || methodHasAttribute typeof<ExportAttribute> x)
                |> Array.iter (fun meth ->
                    if methodHasAttribute typeof<ExportAttribute> meth then
                        let del = TypeDefinition ("", meth.Name + "Delegate", TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable, delType)

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

                        exportedMeths.Add (meth)
                        exportedDels.Add (del)

                        // ******

                        let meth = 
                            MethodDefinition (
                                sprintf "_ferop_set_%s" meth.Name,
                                MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PInvokeImpl ||| MethodAttributes.HideBySig,
                                voidType)

                        meth.IsPInvokeImpl <- true
                        meth.IsPreserveSig <- true
                        meth.HasSecurity <- true
                        meth.PInvokeInfo <-
                            PInvokeInfo (PInvokeAttributes.CallConvCdecl ||| PInvokeAttributes.CharSetAnsi, sprintf "%s__%s" x.Name meth.Name, mref)
                        meth.Parameters.Add (ParameterDefinition ("ptr", ParameterAttributes.None, del))

                        let customAttr = CustomAttribute (importAttrCtor)
                        meth.CustomAttributes.Add (customAttr)

                        x.Methods.Add meth

                        exportedSetMeths.Add (meth)
                    else
                        ())

                let fields =
                    exportedDels
                    |> Seq.map (fun del ->
                        let field = FieldDefinition ("_" + del.Name, FieldAttributes.Public ||| FieldAttributes.Static, del)
                        x.Fields.Add field
                        field)
                    |> Array.ofSeq

                let methAttrs = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName ||| MethodAttributes.Static;
                let meth = new MethodDefinition(".cctor", methAttrs, voidType);

                exportedMeths
                |> Seq.iteri (fun i emeth ->
                    let edel = exportedDels.[i]
                    let esetmeth = exportedSetMeths.[i]
                    let field = fields.[i]

                    let edelctor =
                        edel.Methods
                        |> Seq.find (fun x -> x.IsConstructor)

                    meth.Body.Instructions.Add (Cil.Instruction.Create (Cil.OpCodes.Ldnull))
                    meth.Body.Instructions.Add (Cil.Instruction.Create (Cil.OpCodes.Ldftn, emeth))
                    meth.Body.Instructions.Add (Cil.Instruction.Create (Cil.OpCodes.Newobj, edelctor))
                    meth.Body.Instructions.Add (Cil.Instruction.Create (Cil.OpCodes.Stsfld, field))
                    meth.Body.Instructions.Add (Cil.Instruction.Create (Cil.OpCodes.Ldsfld, field))
                    meth.Body.Instructions.Add (Cil.Instruction.Create (Cil.OpCodes.Call, esetmeth))
                )
                meth.Body.Instructions.Add (Cil.Instruction.Create (Cil.OpCodes.Ret))
                x.Methods.Add meth
            )

            m.Write (assemblyPath + "tmp")
        )


        let load (x: string) = Assembly.ReflectionOnlyLoadFrom (x)
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
                let name = makeDllName x.Name Platform.Auto

                let mref =
                    match
                        asmDef.MainModule.ModuleReferences
                        |> Seq.tryFind (fun x -> x.Name = name) with
                    | Some x -> x
                    | None ->
                        let mref = ModuleReference name
                        asmDef.MainModule.ModuleReferences.Add mref
                        mref

                x.CustomAttributes.Remove (
                    x.CustomAttributes
                    |> Seq.find (fun x -> x.AttributeType.Name.Contains ("Ferop"))) |> ignore

                x.Methods
                |> Array.ofSeq
                |> Array.filter (fun x -> not x.IsConstructor)
                |> Array.filter (fun x -> methodHasAttribute typeof<ImportAttribute> x || methodHasAttribute typeof<ExportAttribute> x)
                |> Array.iter (fun meth ->
                    if methodHasAttribute typeof<ExportAttribute> meth then
                        ()
                    else
                        meth.Attributes <- MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PInvokeImpl ||| MethodAttributes.HideBySig
                        meth.IsPInvokeImpl <- true
                        meth.IsPreserveSig <- true
                        meth.HasSecurity <- true
                        meth.PInvokeInfo <-
                            PInvokeInfo (PInvokeAttributes.CallConvCdecl ||| PInvokeAttributes.CharSetAnsi, sprintf "%s_%s" x.Name meth.Name, mref)

                        let customAttr = CustomAttribute (unSecuAttrCtor)
                        meth.CustomAttributes.Add (customAttr)
                )
            )
            m.Write (assemblyPath)
        )

        classes asm
        |> List.iter (fun m ->
            let modul = makeModule asmDef.MainModule.Architecture m
            let cgen = makeCGen modul
            compileModule targetDirectory modul Platform.Auto cgen)

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

        try

            this.Log.LogMessage ("AppDomain Created.")
            let proxy : Proxy = appDomain.CreateInstanceFromAndUnwrap (currentAsm.Location, typeof<Proxy>.FullName) :?> Proxy
            this.Log.LogMessage ("Executing...")
            proxy.Execute (this.AssemblyPath, this.References, this.TargetDirectory)
            this.Log.LogMessage ("Done.")
        finally
            AppDomain.Unload appDomain
            this.Log.LogMessage ("AppDomain Unloaded.")            
        true
