module Ferop.Compiler

open System
open System.IO
open System.Reflection

open Ferop.Core
open Ferop.Internal

[<RequireQualifiedAccess>]
module Ferop =
    let compileDynamic name outputDir asm =
        let dllName = Path.GetFileName (Path.ChangeExtension (name, ".dll"))
        processAssembly outputDir dllName asm
    
    let compile name outputDir asm =
        let asm = compileDynamic name outputDir asm
        let asmName = asm.GetName ()
        asm.Save (asmName.Name)
        Path.Combine (outputDir, asmName.Name)