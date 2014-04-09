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

// http://wiki.libsdl.org/SDL_EventType
let shouldQuit () = Native.App.shouldQuit ()


let torad = 0.0174532925f

let makeEndpoint degrees length (v: vec2) = 
    vec2 (v.X + length * cos (degrees * torad), v.Y + length * sin (degrees * torad))

let makeDrawLine degrees length (line: DrawLine) = DrawLine (line.Y, makeEndpoint degrees length line.Y)

let makeLines degrees length (line: DrawLine) =

    let rec makeLines degrees length (lines: DrawLine list) n cont =
        match n with
        | 14 -> lines @ cont []
        | _ ->
            let ldeg = degrees + 20.f
            let rdeg = degrees - 20.f
            let ll = makeDrawLine ldeg length lines.Head
            let rl = makeDrawLine rdeg length lines.Head
      
            makeLines ldeg (length * 0.7f) (ll :: lines) (n + 1) (fun x ->
                makeLines rdeg (length * 0.7f) (rl :: x) (n + 1) (fun y -> cont y))

    makeLines degrees length [line] 0 (fun x -> x)
        

[<EntryPoint>]
let main args =
    let app = init ()

    let beginPoint = vec2 (0.f, -1.f)
    let endPoint = vec2 (0.f, -0.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let drawLines = 
        makeLines 90.f (0.4f) drawLine
        |> Array.ofList

    let vbo = makeVbo drawLines

    loadShaders ()

    while not <| shouldQuit () do
        clear ()
        drawVbo drawLines vbo
        draw app

    exit (app)
