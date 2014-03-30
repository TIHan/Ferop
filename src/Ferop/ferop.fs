module Ferop.Code

type FeropAttribute = ReflectedDefinitionAttribute

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type CHeaderAttribute (name: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type CFlagsOsxAttribute (flags: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class)>]
type CLibsOsxAttribute (libs: string) =
    inherit System.Attribute ()

[<System.AttributeUsageAttribute (System.AttributeTargets.Class, AllowMultiple = true)>]
type CIncludeAttribute (headerFile: string) =
    inherit System.Attribute ()

let private errorMsg = "This should not be called directly. Instead, call the generated version."

let C (code: string) =
    code |> ignore
    failwith errorMsg

let CExtern () = failwith errorMsg
