module Ferop.Sample.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime.InteropServices
open System.Threading

open Microsoft.FSharp.NativeInterop

open Ferop.Code

#if DEBUG
type Native = Ferop.FeropProvider<"Ferop.Sample.Native", "bin/Debug", Platform.Auto>
#else
type Native = Ferop.FeropProvider<"Ferop.Sample.Native", "bin/Release", Platform.Auto>
#endif

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

let lrad = 20.f * torad
let rrad = -lrad

let inline makeEndpoint rads length (v: vec2) = vec2 (v.X + length * cos rads, v.Y + length * sin rads)

let inline makeDrawLine rads length (line: DrawLine) = DrawLine (line.Y, makeEndpoint rads length line.Y)

let makeLines degrees length (line: DrawLine) =

    let rec makeLines rads length (lines: DrawLine list) cont = function
        | 16 -> cont lines
        | n ->
            let ldeg = rads + lrad
            let rdeg = rads + rrad
            let ll = makeDrawLine ldeg length lines.Head
            let rl = makeDrawLine rdeg length lines.Head
            let n = n + 1
            let length = length * 0.7f
      
            makeLines ldeg length (ll :: lines) (fun x ->
                makeLines rdeg length (rl :: x) cont n) n

    let rec makeLinesParallel rads length (lines: DrawLine list) cont = function
        | n ->
            let ldeg = rads + lrad
            let rdeg = rads + rrad
            let ll = makeDrawLine ldeg length lines.Head
            let rl = makeDrawLine rdeg length lines.Head
            let n = n + 1
            let length = length * 0.7f

            let f1 = (makeLines ldeg length (ll :: lines) cont)
            let f2 = (makeLines rdeg length (rl :: lines) cont)

            let computations = [| f1; f2 |]

            computations
            |> Array.Parallel.map (fun f -> f n)
            |> Array.reduce (fun x y -> x @ y)

    //makeLines (degrees * torad) length [line] (fun x -> x) 0
    makeLinesParallel (degrees * torad) length [line] (fun x -> x) 0  

[<EntryPoint>]
let main args =
    let app = init ()

    let beginPoint = vec2 (0.f, -1.f)
    let endPoint = vec2 (0.f, -0.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let vbo = makeVbo [||]

    loadShaders ()

    while not <| shouldQuit () do

        let stopwatch = Stopwatch.StartNew ()
        let drawLines = 
            makeLines 90.f (0.4f) drawLine
            |> Array.ofList
        GC.Collect (2)
        stopwatch.Stop ()

        printfn "%A" stopwatch.ElapsedMilliseconds

        clear ()
        drawVbo drawLines vbo
        draw app

    exit (app)
