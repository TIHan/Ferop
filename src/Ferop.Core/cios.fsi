[<RequireQualifiedAccess>]
module internal Ferop.CiOS

open Core
open CGeneration

val compileModule : string -> FeropModule -> CGen -> unit