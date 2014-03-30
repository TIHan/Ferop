module internal Ferop.Core

open System
open System.Reflection

type Parameter = Parameter of string * Type

type Function = Function of string * Type * Parameter list * string

let findParameters (meth: MethodBase) =
    meth.GetParameters ()
    |> List.ofArray
    |> List.map (fun x -> Parameter (x.Name, x.ParameterType))
