module Ferop.Sample.Main

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

    makeLines (degrees * torad) length [line] (fun x -> x) 0
    //makeLinesParallel (degrees * torad) length [line] (fun x -> x) 0 

// http://gafferongames.com/game-physics/fix-your-timestep/
module GameLoop =
    type private GameLoop<'T> = { 
        State: 'T
        Time: float
        LastTime: float
        Accumulator: float }

    let start (state: 'T) (update: float -> float -> 'T -> 'T) (render: float -> 'T -> 'T) =
        let targetInterval = 1000. / 30.

        let stopwatch = Stopwatch.StartNew ()
        let inline time () = stopwatch.Elapsed.TotalMilliseconds

        let rec loop gl =
            let currentTime = time ()
            let deltaTime =
                match currentTime - gl.LastTime with
                | x when x > 250. -> 250.
                | x -> x

            let accumulator = gl.Accumulator + deltaTime

            let rec processUpdate gl =
                if gl.Accumulator >= targetInterval
                then
                    processUpdate
                        { gl with 
                            State = update gl.Time targetInterval gl.State
                            Time = gl.Time + targetInterval
                            Accumulator = gl.Accumulator - targetInterval }
                else
                    gl
                         
            let gl = 
                processUpdate 
                    { gl with 
                        LastTime = currentTime
                        Accumulator = accumulator }       

            loop 
                { gl with 
                    State = render (gl.Accumulator / deltaTime) gl.State }

        loop
            { State = state
              Time = 0.
              LastTime = 0.
              Accumulator = 0. }

[<EntryPoint>]
let main args =
    let app = init ()

    let beginPoint = vec2 (0.f, -1.f)
    let endPoint = vec2 (0.f, -0.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let vbo = makeVbo [||]

    loadShaders ()

    let random = Random (Environment.TickCount)

    GameLoop.start [||] 
        (fun _ _ _ ->
            GC.Collect (2)
            makeLines 90.f (0.4f) drawLine
            |> Array.ofList) 
        (fun _ drawLines ->
            GC.Collect (2)
            clear ()
            drawVbo drawLines vbo
            draw app
            drawLines)

    exit (app)
