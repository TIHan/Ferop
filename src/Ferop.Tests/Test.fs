module Ferop.Tests

open System
open FsUnit
open NUnit.Framework

#if DEBUG
type Native = Ferop.FeropProvider<"Ferop.Tests.Native", "bin/Debug">
#else
type Native = Ferop.FeropProvider<"Ferop.Tests.Native", "bin/Release">
#endif


[<Test>]
let ``with a byte value of 123, should pass and return the same value`` () =
    Native.Tests.testByte (123uy)
    |> should equal 123uy

