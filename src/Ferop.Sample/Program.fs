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
    let app = Native.App.init ()
    Thread.Sleep (2000)

    Native.App.exit (app)
