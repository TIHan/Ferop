module FSharp.Interop.FeropCompiler

open System
open System.IO
open System.Security
open System.Reflection
open System.Reflection.Emit
open System.Runtime.InteropServices

open FSharp.Interop.Ferop
open FSharp.Interop.FeropInternal.Core

[<RequireQualifiedAccess>]
module Ferop =
    val compileDynamic : name: string -> outputPath: string -> dllPath: string -> canCompileModule: bool -> platform: Platform -> asm: Assembly -> AssemblyBuilder

    val compile : name: string -> outputPath: string -> dllPath: string -> canCompileModule: bool -> platform: Platform -> asm: Assembly -> string