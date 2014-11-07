module FSharp.Interop.Ferop

open System.Runtime.CompilerServices

type Platform =
    | Auto = 0
    | Win = 1
    | Linux = 2
    | Osx = 3
    | AppleiOS = 4

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

[<MethodImpl (MethodImplOptions.NoInlining)>]
let C (text: string) = failwith "This should not be called directly. Call the generated version."