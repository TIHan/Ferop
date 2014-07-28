module Ferop.Code

type Platform =
    | Auto = 0
    | Win = 1
    | Linux = 2
    | Osx = 3
    | AppleiOS = 4

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type ClangFlagsOsxAttribute (flags: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type ClangLibsOsxAttribute (libs: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type MsvcLibsWinAttribute (libs: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type MsvcIncludesWinAttribute (libs: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class, AllowMultiple = true)>]
type IncludeAttribute (headerFile: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Method)>]
type ExportAttribute () =
    inherit System.Attribute ()

let private errorMsg = "This should not be called directly. Instead, call the generated version."

let C (code: string) =
    code |> ignore
    failwith errorMsg