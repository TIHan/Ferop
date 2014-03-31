module Ferop.Sample.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime.InteropServices
open System.Threading

type Native = Ferop.FeropProvider<"Ferop.Sample.Native", "bin/Debug">

[<EntryPoint>]
let main args =
    printfn "%A" (Native.App.test2 (1337))
    0
