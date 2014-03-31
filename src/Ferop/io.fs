module FSharp.Control.IO

type IO<'a> = private IO of (unit -> 'a)
 
type IOBuilder () =
    member this.Bind (IO x : IO<'a>, f: 'a -> IO<'b>) : IO<'b> = 
        f (x ())

    member this.Bind (x: IO<'a> list, f: 'a list -> IO<'b>) : IO<'b> =
        f (x |> List.map (fun (IO x) -> x ()))

    member this.Delay (f: unit -> IO<'a>) : IO<'a> = 
        IO (fun _ -> match f () with | IO x -> x ())

    member this.Return (x: 'a) : IO<'a> =
        IO (fun _ -> x)

    member this.ReturnFrom (IO x : IO<'a>) : IO<'a> =
        IO (fun _ -> x ())

    member this.Zero () : IO<unit> =
        IO (fun _ -> ())
 
let io = IOBuilder ()

[<RequireQualifiedAccess>]
module IO =
    let run (IO x) = x ()
