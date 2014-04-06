module internal Ferop.Core

open System
open System.Reflection
open System.Reflection.Emit
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
    Functions: MethodInfo list } with

    member this.IncludeAttributes =
        this.Attributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<IncludeAttribute>)

    member this.ClangFlagsOsxAttribute =
        this.Attributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<ClangFlagsOsxAttribute>)
        |> Seq.exactlyOne

    member this.ClangLibsOsxAttribute =
        this.Attributes
        |> Seq.filter (fun x -> x.AttributeType = typeof<ClangLibsOsxAttribute>)
        |> Seq.exactlyOne

    member this.Includes =
        this.IncludeAttributes
        |> Seq.map (fun x -> Seq.exactlyOne x.ConstructorArguments)
        |> Seq.map (fun x -> "#include " + (x.Value :?> string))
        |> Seq.reduce (fun x y -> x + "\n" + y)

    member this.ClangFlagsOsx =
        let attr = this.ClangFlagsOsxAttribute
        let args = Seq.exactlyOne attr.ConstructorArguments
        args.Value :?> string

    member this.ClangLibsOsx =
        let attr = this.ClangLibsOsxAttribute
        let args = Seq.exactlyOne attr.ConstructorArguments
        args.Value :?> string

let makeModule (typ: Type) =
    let name = typ.Name
    let fullName = typ.FullName
    let shortName = typ.Name
    let attrs = typ.CustomAttributes |> List.ofSeq
    let funcs = Type.moduleFunctions typ

    { Name = name; FullName = shortName; Attributes = attrs; Functions = funcs }

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