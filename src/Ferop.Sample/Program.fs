module Ferop.Sample.Main

open System
open System.IO
open System.Diagnostics
open System.Reflection
open System.Runtime
open System.Runtime.InteropServices
open System.Threading
open System.Collections.Concurrent

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

let exit app = Native.App.exit app

let pollInputEvents () = Native.App.pollInputEvents ()

let torad = 0.0174532925f

let lrad = 20.f * torad
let rrad = -lrad

let inline makeEndpoint rads length (v: vec2) = vec2 (v.X + length * cos rads, v.Y + length * sin rads)

let inline makeDrawLine rads length (line: DrawLine) = DrawLine (line.Y, makeEndpoint rads length line.Y)

let makeLines degrees length (line: DrawLine) =

    let rec makeLines rads length (lines: DrawLine list) cont = function
        | 14 -> cont lines
        | n ->
            let ldeg = rads + lrad
            let rdeg = rads + rrad
            let ll = makeDrawLine ldeg length lines.Head
            let rl = makeDrawLine rdeg length lines.Head
            let n = n + 1
            let length = length * 0.7f
      
            makeLines ldeg length (ll :: lines) (fun x ->
                makeLines rdeg length (rl :: x) cont n) n

    let makeLinesParallel rads length (lines: DrawLine list) cont = function
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

type MainState<'T> = {
    Next: 'T
    Current: 'T
    Previous: 'T
    LastTime: int64
    Accumulator: int64
    NextAccumulator: int64 }

type StateUpdatePriority =
    | Normal
    | Immediate

type StateUpdate<'T> = StateUpdate of StateUpdatePriority * 'T

type StateMachine<'State> (init, normalUpdate, execute) =
    let targetInterval = (1000. / 180.) * 10000. |> int64
    let nextInterval = (1000. / 30.) * 10000. |> int64

    let queue = ConcurrentQueue<StateUpdate<'State>> ()

    let thread =
        new Thread (fun () ->
            let stopwatch = Stopwatch.StartNew ()
            let time () = stopwatch.Elapsed.Ticks

            let rec tryUpdate (main: MainState<'State>) =
                if queue.IsEmpty then
                    None
                else
                    let success, msg = queue.TryDequeue ()

                    if not success
                    then tryUpdate main
                    else

                    match msg with
                    | StateUpdate (Normal, state) ->
                        if not queue.IsEmpty then
                            tryUpdate main
                        else
                            { main with Next = normalUpdate main.Next state } |> Some

                    | StateUpdate (Immediate, state) ->
                        let main = { main with Next = state; Current = state; Previous = state }
                        if not queue.IsEmpty then
                            tryUpdate main
                        else
                            Some main

            let rec loop (main: MainState<'State>) =
                Thread.Sleep (0)

                let currentTime = time ()
                let deltaTime = currentTime - main.LastTime
                let nextAcc = main.NextAccumulator + deltaTime

                let acc =
                    if main.Accumulator > targetInterval
                    then targetInterval
                    else main.Accumulator + deltaTime

                let updated = tryUpdate main
                let main =
                    match updated with
                    | None -> main
                    | Some x -> x

                let rec processNext main =
                    if main.NextAccumulator >= nextInterval then
                        processNext 
                            { main with 
                                Current = main.Next
                                Previous = main.Current
                                NextAccumulator = main.NextAccumulator - nextInterval }
                    else 
                        main

                let main = processNext { main with NextAccumulator = nextAcc }

                if acc >= targetInterval then
                    execute (float main.NextAccumulator / float nextInterval) main.Previous main.Current
                    loop { main with LastTime = currentTime; Accumulator = acc - targetInterval }
                else
                    loop { main with LastTime = currentTime; Accumulator = acc }

            let initial = init ()
            loop { Current = initial; Next = initial; Previous = initial; LastTime = 0L; Accumulator = 0L; NextAccumulator = 0L }
            |> ignore)
        
    member this.Start () =
        thread.Start ()

    member this.Update priority state =
        queue.Enqueue (StateUpdate (priority, state))

module GameLoop =
    type private GameLoop<'T> = { 
        State: 'T
        LastTime: int64
        Time: int64
        Accumulator: int64 }

    let start (state: 'T) (pre: unit -> unit) (update: int64 -> int64 -> 'T -> 'T) =
        let targetUpdateInterval = (1000. / 30.) * 10000. |> int64
        let skip = (1000. / 5.) * 10000. |> int64

        let stopwatch = Stopwatch.StartNew ()
        let inline time () = stopwatch.Elapsed.Ticks

        let rec loop gl =
            let currentTime = time ()
            let deltaTime = currentTime - gl.LastTime
            let acc = 
                match gl.Accumulator + deltaTime with
                | x when x > skip -> skip
                | x -> x

            let rec processUpdate gl =
                if gl.Accumulator >= targetUpdateInterval
                then
                    let state = update gl.Time targetUpdateInterval gl.State
                    //printfn "%A" (TimeSpan.FromTicks(time () - currentTime).TotalMilliseconds)
                    processUpdate
                        { gl with 
                            State = state
                            Time = gl.Time + targetUpdateInterval
                            Accumulator = gl.Accumulator - targetUpdateInterval }
                else
                    { gl with LastTime = currentTime }

            pre ()
       
            { gl with Accumulator = acc }
            |> processUpdate
            |> loop

        loop
            { State = state
              LastTime = 0L
              Time = 0L
              Accumulator = targetUpdateInterval }

type Client = {
    Application: Application
    Vbo: int
    Data: DrawLine [] } with

    static member Default =
        { Application = Unchecked.defaultof<Application>
          Vbo = 0
          Data = [||] }

[<EntryPoint>]
let main args =
    Native.App.initSystems ()

    let beginPoint = vec2 (0.f, -1.f)
    let endPoint = vec2 (0.f, -0.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let inline lerp x y t = x + (y - x) * t

    let window = Native.App.createWindow ()

    let testStateM = 
        StateMachine (
            (fun () ->
                let app = Native.App.createApp (window)
                let client = { Client.Default with Application = app; Vbo = makeVbo [||] }
                loadShaders ()
                client), 
            (fun main state -> { main with Data = state.Data }),
            fun t prev current ->
                let t = single t

                let lerpedDrawLines =
                    if prev.Data.Length <> current.Data.Length
                    then prev.Data
                    else
                        (prev.Data, current.Data)
                        ||> Array.map2 (fun prev x ->
                            DrawLine (
                                vec2 (lerp prev.X.X x.X.X t, lerp prev.X.Y x.X.Y t),
                                vec2 (lerp prev.Y.X x.Y.X t, lerp prev.Y.Y x.Y.Y t)))
                clear ()
                drawVbo lerpedDrawLines current.Vbo
                draw current.Application)

    testStateM.Start ()

    let refLength = ref 0.4f
    let refIsUpPressed = ref false
    let refIsDownPressed = ref false

    GameLoop.start [||] 
        (fun () ->
            Thread.Sleep (1)
            pollInputEvents ())
        (fun _ time _ ->
            match Input.processInput () with
            | [] -> ()
            | xs ->
                xs
                |> List.iter (fun x ->
                    match x with
                    | KeyPressed key ->
                        match key with
                        | 'R' -> refIsUpPressed := true
                        | 'Q' -> refIsDownPressed := true
                        | _ -> ()
                    | KeyReleased key -> 
                        match key with
                        | 'R' -> refIsUpPressed := false
                        | 'Q' -> refIsDownPressed := false
                        | _ -> ())

            let length = !refLength
            let length =
                if !refIsUpPressed then
                    length + (0.0005f) * single (TimeSpan.FromTicks(time).TotalMilliseconds)
                else length
                            
            let length =
                if !refIsDownPressed then
                    length - (0.0005f) * single (TimeSpan.FromTicks(time).TotalMilliseconds)
                else length
                       
            refLength := length
            let lines = 
                makeLines 90.f (length) drawLine
                |> Array.ofList
            testStateM.Update Normal { Client.Default with Data = lines }
            lines)
    0