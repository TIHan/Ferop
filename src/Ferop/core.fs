module internal FSharp.Interop.FeropInternal.Core

open System
open System.IO
open System.Reflection
open System.Diagnostics

open FSharp.Interop.Ferop
open FSharp.Control.IO

type FeropModule = {
    Name: string
    FullName: string
    Attributes: CustomAttributeData list
    Functions: MethodInfo list
    ExportedFunctions: MethodInfo list
    Architecture: Mono.Cecil.TargetArchitecture } with

    member this.ClangFlagsOsxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<ClangFlagsOsxAttribute>.FullName)

    member this.ClangLibsOsxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<ClangLibsOsxAttribute>.FullName)

    member this.GccFlagsLinuxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<GccFlagsLinuxAttribute>.FullName)

    member this.GccLibsLinuxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<GccLibsLinuxAttribute>.FullName)

    member this.MsvcOptionsWinAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<MsvcOptionsWinAttribute>.FullName)

    member this.IsCpp =
        this.Attributes
        |> Seq.exists (fun x -> x.AttributeType.FullName = typeof<CppAttribute>.FullName)

    member this.Header =
        match
            this.Attributes
            |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<HeaderAttribute>.FullName)
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
    |> Seq.map (fun x -> x.AttributeType.FullName)
    |> Seq.exists ((=)typ.FullName)

let makeModule arch (typ: Type) =
    let name = typ.Name
    let fullName = typ.FullName
    let attrs = typ.CustomAttributes |> List.ofSeq
    let funcs = staticMethods typ
    let normalFuncs = funcs |> List.filter (methodHasAttribute typeof<ImportAttribute>)
    let exportFuncs = funcs |> List.filter (methodHasAttribute typeof<ExportAttribute>)

    { Name = name
      FullName = fullName
      Attributes = attrs
      Functions = normalFuncs
      ExportedFunctions = exportFuncs
      Architecture = arch }

let makeHFilePath path modul = Path.Combine (path, sprintf "%s.h" modul.Name)

let makeCFilePath path modul = Path.Combine (path, sprintf "%s.c" modul.Name)

let makeCppFilePath path modul = Path.Combine (path, sprintf "%s.cpp" modul.Name)

let checkProcessError (p: Process) = 
    if p.ExitCode <> 0 then 
        let msg = p.StandardError.ReadToEnd ()
        let msg2 = p.StandardOutput.ReadToEnd ()
        failwith (msg + "\n" + msg2)

open CConversion
open CGeneration

let makeCConvInfo (modul: FeropModule) = 
    { Name = modul.Name; Functions = modul.Functions; ExportedFunctions = modul.ExportedFunctions; IsCpp = modul.IsCpp }

let makeCGen (modul: FeropModule) =
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