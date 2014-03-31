(*
Copyright (c) 2014 William F. Smith

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*)

module Ferop.TypeProvider

open System
open System.IO
open System.Reflection
open System.Windows.Input

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape

open ProviderImplementation
open ProviderImplementation.ProvidedTypes

[<RequireQualifiedAccess>]
module internal TypeProviderConfig =
    let tryFindAssembly predicate (cfg: TypeProviderConfig) =
        cfg.ReferencedAssemblies |> Array.tryFind predicate
 
[<RequireQualifiedAccess>]
module internal TypeProvider =
    /// Load an assembly file properly for a type provider.
    let loadAssemblyFile fileName = File.ReadAllBytes fileName |> Assembly.Load

[<TypeProvider>]
type FeropTypeProvider (cfg: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let asm = Assembly.GetExecutingAssembly ()
    let ns = this.GetType().Namespace
    let pn = "FeropProvider"

    let tempAsm = ProvidedAssembly (Path.ChangeExtension (Path.GetTempFileName (), ".dll")) 
    let parameters = [
        ProvidedStaticParameter ("refName", typeof<string>)
        ProvidedStaticParameter ("relativeDir", typeof<string>)]

    do
        let def = ProvidedTypeDefinition (asm, ns, pn, Some typeof<obj>, IsErased = false) 
        tempAsm.AddTypes [def]
        def.DefineStaticParameters (parameters, this.GenerateTypes)
        this.AddNamespace(ns, [def])

    override this.ResolveAssembly args =
        let name = System.Reflection.AssemblyName (args.Name)
        let existingAssembly = 
            System.AppDomain.CurrentDomain.GetAssemblies ()
            |> Seq.tryFind (fun a -> System.Reflection.AssemblyName.ReferenceMatchesDefinition (name, a.GetName ()))
        match existingAssembly with
        | Some a -> a
        | None -> null       

    /// FindAssembly
    member internal this.FindAssembly fileName =
        match cfg |> TypeProviderConfig.tryFindAssembly (fun fullPath -> Path.GetFileNameWithoutExtension fullPath = fileName) with
        | None -> failwithf "Invalid assembly name %s. Pick from the list of referenced assemblies." fileName
        | Some masmFileName -> TypeProvider.loadAssemblyFile masmFileName

    /// GenerateTypes
    member internal this.GenerateTypes (typeName: string) (args: obj[]) =
        let refName = args.[0] :?> string
        let relativeDir = args.[1] :?> string

        let name = Path.GetTempFileName ()
        let outputPath = Path.Combine (cfg.ResolutionFolder, relativeDir)
        let dllPath = Path.GetTempPath ()
        let refAsm = this.FindAssembly refName

        let dasmLocation = Ferop.Compiler.Ferop.compile name outputPath dllPath refAsm
        let dasm = Assembly.LoadFrom (dasmLocation)

        let def = ProvidedTypeDefinition (asm, ns, typeName, Some typeof<obj>, IsErased = false) 
        tempAsm.AddTypes [def]

        def.AddMembers <| (dasm.GetTypes () |> List.ofArray)

        def

[<assembly:TypeProviderAssembly>]
do ()
