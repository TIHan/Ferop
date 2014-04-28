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

open Ferop.Helpers
open Ferop.Code

open FSharp.Control.IO

type Module = {
    Name: string
    FullName: string
    Attributes: CustomAttributeData list
    Functions: MethodInfo list
    ExportedFunctions: MethodInfo list } with

    member this.IncludeAttributes =
        this.Attributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<IncludeAttribute>)

    member this.ClangFlagsOsxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<ClangFlagsOsxAttribute>)

    member this.ClangLibsOsxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<ClangLibsOsxAttribute>)

    member this.MsvcLibsWinAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<MsvcLibsWinAttribute>)

    member this.MsvcIncludesWinAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<MsvcIncludesWinAttribute>)

    member this.Includes =
        match Seq.isEmpty this.IncludeAttributes with
        | true -> ""
        | _ ->
            this.IncludeAttributes
            |> Seq.map (fun x -> Seq.exactlyOne x.ConstructorArguments)
            |> Seq.map (fun x -> "#include " + (x.Value :?> string))
            |> Seq.reduce (fun x y -> x + "\n" + y)

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

    member this.MsvcLibsWin =
        match this.MsvcLibsWinAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.MsvcIncludesWin =
        match this.MsvcIncludesWinAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

let makeModule (typ: Type) =
    let name = typ.Name
    let fullName = typ.FullName
    let shortName = typ.Name
    let attrs = typ.CustomAttributes |> List.ofSeq
    let funcs = Type.moduleFunctions typ
    let normalFuncs = funcs |> List.filter (fun x -> not (MethodInfo.hasAttribute typeof<ExportAttribute> x))
    let exportFuncs = funcs |> List.filter (MethodInfo.hasAttribute typeof<ExportAttribute>)

    { Name = name; FullName = shortName; Attributes = attrs; Functions = normalFuncs; ExportedFunctions = exportFuncs }

let makeHeaderIncludes (modul: Module) = modul.Includes

let makeHFilePath path modul = Path.Combine (path, sprintf "%s.h" modul.Name)

let makeCFilePath path modul = Path.Combine (path, sprintf "%s.c" modul.Name)

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
    meth

open Ferop.CConversion
open Ferop.CGen

let makeFsModule (modul: Module) = 
    { Name = modul.Name; Functions = modul.Functions; ExportedFunctions = modul.ExportedFunctions  }

let makeCGen outputPath (modul: Module) =
    let env = makeCEnv <| makeFsModule modul
    generate env modul.Includes

let writeCGen outputPath modul cgen = io {
    let hFile = makeHFilePath outputPath modul
    let cFile = makeCFilePath outputPath modul

    File.WriteAllText (hFile, cgen.Header)
    File.WriteAllText (cFile, cgen.Source)
    return hFile, cFile }