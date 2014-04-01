module Ferop.Tests

open System
open FsUnit
open NUnit.Framework

open Ferop

#if DEBUG
type Native = FeropProvider<"Ferop.Tests.Native", "bin/Debug">
#else
type Native = FeropProvider<"Ferop.Tests.Native", "bin/Release">
#endif

[<Test>]
let ``with a byte value of 123, should pass and return the same value`` () =
    Native.Tests.testByte (123uy)
    |> should equal 123uy

[<Test>]
let ``with a sbyte value of 123, should pass and return the same value`` () =
    Native.Tests.testSbyte (123y)
    |> should equal 123y
