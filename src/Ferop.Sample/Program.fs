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

let clear () = Native.App.clear ()

let draw app = Native.App.draw app

let loadShaders () =
    let mutable vertexFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("v.vertex"))
    let mutable fragmentFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("f.fragment"))

    // Fixed
    let vh = GCHandle.Alloc (vertexFile, GCHandleType.Pinned)
    let vp = vh.AddrOfPinnedObject ()

    let fh = GCHandle.Alloc (fragmentFile, GCHandleType.Pinned)
    let fp = fh.AddrOfPinnedObject ()

    Native.App.loadShaders (vp, fp)

    vh.Free ()
    fh.Free ()
    // End Fixed

let makeVbo (drawLines: DrawLine []) = 
    let h = GCHandle.Alloc (drawLines, GCHandleType.Pinned)
    let p = h.AddrOfPinnedObject ()
    let vbo = Native.App.generateVbo (drawLines.Length * sizeof<DrawLine>, p)
    h.Free ()
    vbo

let drawVbo (drawLines: DrawLine []) vbo =
    let h = GCHandle.Alloc (drawLines, GCHandleType.Pinned)
    let p = h.AddrOfPinnedObject ()
    Native.App.drawVbo (drawLines.Length * sizeof<DrawLine>, p, vbo)
    h.Free ()

let init () = Native.App.init ()

let exit app = Native.App.exit app

[<EntryPoint>]
let main args =
    let app = init ()

    let beginPoint = vec2 (0.f, 0.f)
    let endPoint = vec2 (0.5f, 1.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let drawLines = [|drawLine|]

    let vbo = makeVbo drawLines

    loadShaders ()

    clear ()
    drawVbo drawLines vbo
    draw app

    Thread.Sleep (2000)

    exit (app)
