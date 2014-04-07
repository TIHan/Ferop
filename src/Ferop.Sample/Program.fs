module Ferop.Sample.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime.InteropServices
open System.Threading

open Microsoft.FSharp.NativeInterop

type Native = Ferop.FeropProvider<"Ferop.Sample.Native", "bin/Debug">

#nowarn "9"
#nowarn "51"

[<Struct>]
type vec2 =
    val X : single
    val Y : single

    new (x, y) = { X = x; Y = y }

[<Struct>]
type DrawLine =
    val X : vec2
    val Y : vec2

    new (x, y) = { X = x; Y = y }

[<EntryPoint>]
let main args =
    let app = Native.App.init ()

    let beginPoint = vec2 (0.f, 0.f)
    let endPoint = vec2 (0.5f, 1.5f)
    let mutable drawLine = DrawLine (beginPoint, endPoint)

    let mutable vertexFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("v.vertex"))
    let mutable fragmentFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("f.fragment"))

    let vbo = Native.App.generateVbo (sizeof<DrawLine>, NativePtr.toNativeInt &&drawLine)

    // Fixed
    let vh = GCHandle.Alloc (vertexFile, GCHandleType.Pinned)
    let vp = vh.AddrOfPinnedObject ()

    let fh = GCHandle.Alloc (fragmentFile, GCHandleType.Pinned)
    let fp = fh.AddrOfPinnedObject ()

    Native.App.loadShaders (vp, fp)

    vh.Free ()
    fh.Free ()
    // End Fixed

    Native.App.clear ()

    Native.App.drawVbo (sizeof<DrawLine>, NativePtr.toNativeInt &&drawLine, vbo)

    Native.App.draw app

    Thread.Sleep (2000)

    Native.App.exit (app)
