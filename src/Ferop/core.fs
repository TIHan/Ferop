module internal FSharp.Interop.FeropInternal.Core

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Diagnostics
open System.Security
open System.Runtime.InteropServices

open FSharp.Interop.Ferop
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

    member this.GccFlagsLinuxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<GccFlagsLinuxAttribute>)

    member this.GccLibsLinuxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<GccLibsLinuxAttribute>)

    member this.MsvcOptionsWinAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType = typeof<MsvcOptionsWinAttribute>)

    member this.Is64bit =
        this.Attributes
        |> Seq.exists (fun x -> x.AttributeType = typeof<Cpu64bitAttribute>)

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

    member this.GccFlagsLinux =
        match this.GccFlagsLinuxAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.GccLibsLinux =
        match this.GccLibsLinuxAttribute with
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
    let attrs = typ.CustomAttributes |> List.ofSeq
    let funcs = staticMethods typ
    let normalFuncs = funcs |> List.filter (fun x -> not (methodHasAttribute typeof<ExportAttribute> x))
    let exportFuncs = funcs |> List.filter (methodHasAttribute typeof<ExportAttribute>)

    { Name = name; FullName = fullName; Attributes = attrs; Functions = normalFuncs; ExportedFunctions = exportFuncs }

let makeHFilePath path modul = Path.Combine (path, sprintf "%s.h" modul.Name)

let makeCFilePath path modul = Path.Combine (path, sprintf "%s.c" modul.Name)

let makeCppFilePath path modul = Path.Combine (path, sprintf "%s.cpp" modul.Name)

let checkProcessError (p: Process) = if p.ExitCode <> 0 then failwith (p.StandardError.ReadToEnd ())

let addMethodAttribute<'T> (meth: MethodBuilder) (ctorTypes: Type []) (ctorArgs: obj []) =
    let attributeType = typeof<'T>
    let attributeConstructorInfo = attributeType.GetConstructor (ctorTypes)
    let attributeBuilder = CustomAttributeBuilder (attributeConstructorInfo, ctorArgs)
    meth.SetCustomAttribute (attributeBuilder)

let addTypeAttribute<'T> (tb: TypeBuilder) (ctorTypes: Type []) (ctorArgs: obj []) =
    let attributeType = typeof<'T>
    let attributeConstructorInfo = attributeType.GetConstructor (ctorTypes)
    let attributeBuilder = CustomAttributeBuilder (attributeConstructorInfo, ctorArgs)
    tb.SetCustomAttribute (attributeBuilder)

open CConversion
open CGeneration

let makeCConvInfo (modul: Module) = 
    { Name = modul.Name; Functions = modul.Functions; ExportedFunctions = modul.ExportedFunctions; IsCpp = modul.IsCpp }

let makeCGen (modul: Module) =
    let env = makeCEnv <| makeCConvInfo modul
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