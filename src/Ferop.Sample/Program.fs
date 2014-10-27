﻿module Ferop.Sample.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime
open System.Runtime.InteropServices
open System.Threading

open Microsoft.FSharp.NativeInterop

open FSharp.Interop
open Ferop.Sample

#if DEBUG
type Native = CProvider<"Ferop.Sample.Native", "bin/Debug">
#else
type Native = CProvider<"Ferop.Sample.Native", "bin/Release">
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

let init () = 
    GCSettings.LatencyMode <- GCLatencyMode.Batch
    Native.App.init ()

let exit app = Native.App.exit app

let shouldQuit () = Native.App.shouldQuit ()

let torad = 0.0174532925f

let lrad = 20.f * torad
let rrad = -lrad

type RandomSingle private () =
    [<DefaultValue>]
    [<ThreadStatic>]
    static val mutable private random : Random

    static member Random
        with get () = RandomSingle.random
        and set value = RandomSingle.random <- value

let randomSingle () =
    if RandomSingle.Random = Unchecked.defaultof<Random> then
        RandomSingle.Random <- Random (Environment.TickCount)
    (single (RandomSingle.Random.NextDouble()) * (0.9f - 0.5f) + 0.5f)

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
            let length = length * randomSingle ()
      
            makeLines ldeg length (ll :: lines) (fun x ->
                makeLines rdeg length (rl :: x) cont n) n

    let makeLinesParallel rads length (lines: DrawLine list) cont = function
            | n ->
                let ldeg = rads + lrad
                let rdeg = rads + rrad
                let ll = makeDrawLine ldeg length lines.Head
                let rl = makeDrawLine rdeg length lines.Head
                let n = n + 1
                let length = length * randomSingle ()

                let f1 = (makeLines ldeg length (ll :: lines) cont)
                let f2 = (makeLines rdeg length (rl :: lines) cont)

                let computations = [| f1; f2 |]

                computations
                |> Array.Parallel.map (fun f -> f n)
                |> Array.reduce (fun x y -> x @ y)

    makeLines (degrees * torad) length [line] (fun x -> x) 0
    //makeLinesParallel (degrees * torad) length [line] (fun x -> x) 0 

// http://gafferongames.com/game-physics/fix-your-timestep/
module GameLoop =
    type private GameLoop<'T> = { 
        State: 'T
        PreviousState: 'T
        UpdateTime: float
        LastTime: float
        Accumulator: float
        RenderInterval: float
        RenderAccumulator: float }

    let start (state: 'T) (update: float -> float -> 'T -> 'T) (render: float -> 'T -> 'T -> 'T) =
        let targetUpdateInterval = 1000. / 30.
        let targetRenderInterval = 1000. / 120.
        let skip = 1000. / 25.

        let stopwatch = Stopwatch.StartNew ()
        let inline time () = stopwatch.Elapsed.TotalMilliseconds

        let rec loop gl =
            let currentTime = time ()
            let deltaTime =
                match currentTime - gl.LastTime with
                | x when x > skip -> skip
                | x -> x

            let accumulator = gl.Accumulator + deltaTime
            let renderAccumulator = gl.RenderAccumulator + deltaTime

            let rec processUpdate gl =
                if gl.Accumulator >= targetUpdateInterval
                then
                    let state = update gl.UpdateTime targetUpdateInterval gl.State
                    let renderInterval = time () - currentTime

                    let renderInterval =
                        if renderInterval < targetRenderInterval
                        then targetRenderInterval
                        else renderInterval

                    processUpdate
                        { gl with 
                            State = state
                            PreviousState = gl.State
                            UpdateTime = gl.UpdateTime + targetUpdateInterval
                            Accumulator = gl.Accumulator - targetUpdateInterval
                            RenderInterval = renderInterval }
                else
                    gl

            let processRender gl =
                if gl.RenderAccumulator >= gl.RenderInterval then
                    { gl with 
                        LastTime = currentTime
                        RenderAccumulator = gl.RenderAccumulator - gl.RenderInterval
                        State = render (gl.Accumulator / targetUpdateInterval) gl.PreviousState gl.State }
                else
                    { gl with LastTime = currentTime }
                         
            { gl with Accumulator = accumulator; RenderAccumulator = renderAccumulator }
            |> processUpdate
            |> processRender
            |> loop

        loop
            { State = state
              PreviousState = state
              UpdateTime = 0.
              LastTime = 0.
              Accumulator = 0.
              RenderInterval = 0.
              RenderAccumulator = 0. }

[<EntryPoint>]
let main args =
    let app = init ()

    let beginPoint = vec2 (0.f, -1.f)
    let endPoint = vec2 (0.f, -0.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let vbo = makeVbo [||]

    loadShaders ()

    let inline lerp x y t = x + (y - x) * t

    GameLoop.start [||] 
        (fun _ _ _ ->
            GC.Collect (2)
            makeLines 90.f (0.4f) drawLine
            |> Array.ofList) 
        (fun t prevDrawLines drawLines ->
            let t = single t
            GC.Collect (2)
            let lerpedDrawLines =
                if prevDrawLines.Length <> drawLines.Length
                then prevDrawLines
                else
                    (prevDrawLines, drawLines)
                    ||> Array.map2 (fun prev x ->
                        DrawLine (
                            vec2 (lerp prev.X.X x.X.X t, lerp prev.X.Y x.X.Y t),
                            vec2 (lerp prev.Y.X x.Y.X t, lerp prev.Y.Y x.Y.Y t)))

            clear ()
            drawVbo lerpedDrawLines vbo
            draw app
            drawLines)

    exit (app)
