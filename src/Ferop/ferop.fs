module FSharp.Interop.Ferop

type Platform =
    | Auto = 0
    | Win = 1
    | Linux = 2
    | Osx = 3
    //| AppleiOS = 4

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type FeropAttribute () =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type ClangFlagsOsxAttribute (flags: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type ClangLibsOsxAttribute (libs: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type GccFlagsLinuxAttribute (flags: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type GccLibsLinuxAttribute (libs: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type MsvcOptionsWinAttribute (libs: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type Cpu64bitAttribute () =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type CppAttribute () =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type HeaderAttribute (header: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Method)>]
type ExportAttribute () =
    inherit System.Attribute ()

// Hack to get the F# compiler to not inline the function calling this one.
let inline code (text: string) =
    text |> ignore
    failwith text
    failwith text