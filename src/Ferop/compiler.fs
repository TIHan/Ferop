module Ferop.Compiler

open System
open System.IO
open System.Reflection

open Ferop.Core
open Ferop.Internal

[<RequireQualifiedAccess>]
module Ferop =
    let compileDynamic name outputPath dllPath canCompileModule platform asm =
        let dllName = Path.GetFileName (Path.ChangeExtension (name, ".dll"))
        processAssembly dllName outputPath dllPath canCompileModule platform asm
    
    let compile name outputPath dllPath canCompileModule platform asm =
        let asm = compileDynamic name outputPath dllPath canCompileModule platform asm
        let asmName = asm.GetName ()
        asm.Save (asmName.Name)
        Path.Combine (dllPath, asmName.Name)