module Ferop.Sample.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime.InteropServices
open System.Threading

open Microsoft.FSharp.NativeInterop

open FSharp.Interop.Ferop
open Ferop.Sample

#if DEBUG
type Native = CProvider<"Ferop.Sample.Native", "bin/Debug", Platform.Auto>
#else
type Native = CProvider<"Ferop.Sample.Native", "bin/Release", Platform.Auto>
#endif

#nowarn "9"
#nowarn "51"

let clear () = Native.App.clear ()

let draw app = Native.App.draw app

let loadShaders () =
    let mutable vertexFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("v.vertex"))
    let mutable fragmentFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("f.fragment"))

    Native.App.loadShaders (vertexFile, fragmentFile)

let makeVbo (drawLines: DrawLine []) = 
    let vbo = Native.App.generateVbo (drawLines.Length * sizeof<DrawLine>, drawLines)
    vbo

let drawVbo (drawLines: DrawLine []) vbo =
    Native.App.drawVbo (drawLines.Length * sizeof<DrawLine>, drawLines, vbo)

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
        | 11 -> cont lines
        | n ->
            let ldeg = rads + lrad
            let rdeg = rads + rrad
            let ll = makeDrawLine ldeg length lines.Head
            let rl = makeDrawLine rdeg length lines.Head
            let n = n + 1
            let length = length * 0.7f
      
            makeLines ldeg length (ll :: lines) (fun x ->
                makeLines rdeg length (rl :: x) cont n) n

    makeLines (degrees * torad) length [line] (fun x -> x) 0

[<EntryPoint>]
let main args =
    let app = init ()

    let beginPoint = vec2 (0.f, -1.f)
    let endPoint = vec2 (0.f, -0.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let vbo = makeVbo [||]

    loadShaders ()

    while not <| ((shouldQuit ()) = 1) do

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
