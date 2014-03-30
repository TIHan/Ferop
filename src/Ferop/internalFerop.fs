module internal Ferop.Internal

open System
open System.Reflection
open System.Reflection.Emit

open Microsoft.FSharp.Reflection

open Ferop.Core
open Ferop.Code
open Ferop.Helpers

let createDynamicAssembly (path: string) dllName =
    AppDomain.CurrentDomain.DefineDynamicAssembly (AssemblyName (dllName), Emit.AssemblyBuilderAccess.RunAndSave, path)
    
let processAssembly (path: string) dllName (asm: Assembly) =
    let dasm = createDynamicAssembly path dllName
    let mb = dasm.DefineDynamicModule dllName

    Assembly.modules asm
    |> List.filter (fun x ->
        x.CustomAttributes
        |> Seq.exists (fun x -> x.AttributeType = typeof<FeropAttribute>))
    |> List.map (fun x ->
        let tb = mb.DefineType (x.FullName, TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
        Ferop.C.compileModule path tb x)
    |> ignore

    dasm
