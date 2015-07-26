module internal Ferop.Core

open System
open System.IO
open System.Reflection
open System.Diagnostics

open Ferop

type Platform =
    | Auto = 0
    | Win = 1
    | Linux = 2
    | Osx = 3
    | iOS = 4

type Language =
    | C
    | Cpp
    //| ObjC

type FeropModule = {
    Name: string
    FullName: string
    Attributes: CustomAttributeData list
    Functions: MethodInfo list
    ExportedFunctions: MethodInfo list
    Architecture: Mono.Cecil.TargetArchitecture } with

    member this.ClangOsxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<ClangOsxAttribute>.FullName)

    member this.GccLinuxAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<GccLinuxAttribute>.FullName)

    member this.MsvcOptionsWinAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<MsvcWinAttribute>.FullName)

    member this.ClangiOSAttribute =
        this.Attributes
        |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<ClangiOSAttribute>.FullName)

    member this.Language =
        let isCpp =
            this.Attributes
            |> Seq.exists (fun x -> x.AttributeType.FullName = typeof<CppAttribute>.FullName)
        if isCpp
        then Cpp
        else C

    member this.IsForiOS =
        this.Attributes
        |> Seq.exists (fun x -> x.AttributeType.FullName = typeof<ClangiOSAttribute>.FullName)

    member this.Header =
        match
            this.Attributes
            |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<HeaderAttribute>.FullName)
            with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.Source =
        match
            this.Attributes
            |> Seq.tryFind (fun x -> x.AttributeType.FullName = typeof<SourceAttribute>.FullName)
            with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.ClangFlagsOsx =
        match this.ClangOsxAttribute with
        | None -> ""
        | Some attr ->
            let args = attr.ConstructorArguments.[0]
            args.Value :?> string

    member this.ClangLibsOsx =
        match this.ClangOsxAttribute with
        | None -> ""
        | Some attr ->
            let args = attr.ConstructorArguments.[1]
            args.Value :?> string

    member this.GccFlagsLinux =
        match this.GccLinuxAttribute with
        | None -> ""
        | Some attr ->
            let args = attr.ConstructorArguments.[0]
            args.Value :?> string

    member this.GccLibsLinux =
        match this.GccLinuxAttribute with
        | None -> ""
        | Some attr ->
            let args = attr.ConstructorArguments.[1]
            args.Value :?> string

    member this.MsvcOptionsWin =
        match this.MsvcOptionsWinAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

    member this.ClangFlagsiOS =
        match this.ClangiOSAttribute with
        | None -> ""
        | Some attr ->
            let args = Seq.exactlyOne attr.ConstructorArguments
            args.Value :?> string

let staticMethods (t: Type) =
    t.GetRuntimeMethods ()
    |> Seq.filter (fun x -> 
    x.Name <> "GetType" && 
    x.Name <> "GetHashCode" && 
    x.Name <> "Equals" && 
    x.Name <> "ToString")
    |> Seq.filter (fun x -> x.IsStatic)
    |> List.ofSeq

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

let checkProcessError extra (p: Process) = 
    if p.ExitCode <> 0 then 
        let msg = p.StandardError.ReadToEnd ()
        let msg2 = p.StandardOutput.ReadToEnd ()
        failwith (msg + "\n" + msg2 + "\n" + extra)

open CConversion
open CGeneration

let makeCConvInfo (modul: FeropModule) = 
    { 
        Name = modul.Name
        ImportedFunctions = modul.Functions
        ExportedFunctions = modul.ExportedFunctions
        IsCpp =
            match modul.Language with
            | Cpp -> true
            | _ -> false
    }

let makeCGen (modul: FeropModule) =
    let env = makeCEnv <| makeCConvInfo modul
    generate env modul.Header modul.Source

let writeCGen outputPath modul cgen = async {
    let hFile = makeHFilePath outputPath modul
    let cFile = 
        match modul.Language with
        | C -> makeCFilePath outputPath modul
        | Cpp -> makeCppFilePath outputPath modul

    File.WriteAllText (hFile, cgen.Header)
    File.WriteAllText (cFile, cgen.Source)
    return hFile, cFile }