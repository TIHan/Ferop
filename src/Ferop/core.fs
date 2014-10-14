module internal Ferop.Core

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Diagnostics
open System.Security
open System.Runtime.InteropServices

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open Ferop.Code

open FSharp.Control.IO

type Module = {
    Name: string
    FullName: string
    Attributes: CustomAttributeData list
    Functions: MethodInfo list
    ExportedFunctions: MethodInfo list } with

    member this.ClangFlagsOsxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<ClangFlagsOsxAttribute>)

    member this.ClangLibsOsxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<ClangLibsOsxAttribute>)

    member this.MsvcOptionsWinAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<MsvcOptionsWinAttribute>)

    member this.IsMsvc64bit =
        this.Attributes
        |> Seq.exists (fun x -> x.AttributeType = typeof<Msvc64bitAttribute>)

    member this.IsCpp =
        this.Attributes
        |> Seq.exists (fun x -> x.AttributeType = typeof<CppAttribute>)

    member this.Header =
        match
            this.Attributes
            |> Seq.tryFind (fun x -> x.AttributeType = typeof<HeaderAttribute>)
            with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.ClangFlagsOsx =
        match this.ClangFlagsOsxAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.ClangLibsOsx =
        match this.ClangLibsOsxAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.MsvcOptionsWin =
        match this.MsvcOptionsWinAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

let staticMethods (t: Type) =
    t.GetMethods ()
    |> Array.filter (fun x -> 
    x.Name <> "GetType" && 
    x.Name <> "GetHashCode" && 
    x.Name <> "Equals" && 
    x.Name <> "ToString")
    |> Array.filter (fun x -> x.IsStatic)
    |> List.ofArray

let methodHasAttribute (typ: Type) (meth: MethodInfo) =
    meth.GetCustomAttributesData ()
    |> Seq.map (fun x -> x.AttributeType)
    |> Seq.exists ((=)typ)

let makeModule (typ: Type) =
    let name = typ.Name
    let fullName = typ.FullName
    let shortName = typ.Name
    let attrs = typ.CustomAttributes |> List.ofSeq
    let funcs = staticMethods typ
    let normalFuncs = funcs |> List.filter (fun x -> not (methodHasAttribute typeof<ExportAttribute> x))
    let exportFuncs = funcs |> List.filter (methodHasAttribute typeof<ExportAttribute>)

    { Name = name; FullName = shortName; Attributes = attrs; Functions = normalFuncs; ExportedFunctions = exportFuncs }

let makeHFilePath path modul = Path.Combine (path, sprintf "%s.h" modul.Name)

let makeCFilePath path modul = Path.Combine (path, sprintf "%s.c" modul.Name)

let makeCppFilePath path modul = Path.Combine (path, sprintf "%s.cpp" modul.Name)

let makeDummyCFilePath path = Path.Combine (path, "_ferop_dummy_.c")

let dummyC = ""

let checkProcessError (p: Process) = if p.ExitCode <> 0 then failwith (p.StandardError.ReadToEnd ())

let definePInvokeMethod (tb: TypeBuilder) dllName (func: MethodInfo) =
    let meth = 
        tb.DefinePInvokeMethod (
            func.Name,
            dllName,
            sprintf "%s_%s" func.DeclaringType.Name func.Name,
            MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.PinvokeImpl,
            CallingConventions.Standard,
            func.ReturnType,
            func.GetParameters () |> Array.map (fun x -> x.ParameterType),
            CallingConvention.Cdecl,
            CharSet.Ansi)

    meth.SetImplementationFlags (meth.GetMethodImplementationFlags () ||| MethodImplAttributes.PreserveSig)
    let attributeType = typeof<SuppressUnmanagedCodeSecurityAttribute>
    let attributeConstructorInfo = attributeType.GetConstructor([||])
    let attributeBuilder = CustomAttributeBuilder(attributeConstructorInfo, [||]);
    meth.SetCustomAttribute(attributeBuilder);

#if COPY_PARAMETER_ATTRIBUTES
    func.GetParameters ()
    |> Array.iteri (fun i x ->
        let pb = meth.DefineParameter (x.Position, x.Attributes, x.Name)
        x.CustomAttributes
        |> Seq.iter (fun x -> 
            let at = x.AttributeType
            let aci = x.Constructor
            let cargs = x.ConstructorArguments
            let ab = CustomAttributeBuilder (aci, cargs |> Seq.map (fun y -> y.Value) |> Array.ofSeq)
            pb.SetCustomAttribute ab))
#endif

    meth

open Ferop.CConversion
open Ferop.CGen

let makeFsModule (modul: Module) = 
    { Name = modul.Name; Functions = modul.Functions; ExportedFunctions = modul.ExportedFunctions; IsCpp = modul.IsCpp }

let makeCGen (modul: Module) =
    let env = makeCEnv <| makeFsModule modul
    generate env modul.Header

let writeCGen outputPath modul cgen = io {
    let hFile = makeHFilePath outputPath modul
    let cFile = 
        if modul.IsCpp
        then makeCppFilePath outputPath modul
        else makeCFilePath outputPath modul

    File.WriteAllText (hFile, cgen.Header)
    File.WriteAllText (cFile, cgen.Source)
    return hFile, cFile }