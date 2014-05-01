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
let ``with max value of byte, should pass and return the same value`` () =
    Native.Tests.testByte (Byte.MaxValue)
    |> should equal Byte.MaxValue

[<Test>]
let ``with max value of sbyte, should pass and return the same value`` () =
    Native.Tests.testSByte (SByte.MaxValue)
    |> should equal SByte.MaxValue

[<Test>]
let ``with max value of uint16, should pass and return the same value`` () =
    Native.Tests.testUInt16 (UInt16.MaxValue)
    |> should equal UInt16.MaxValue

[<Test>]
let ``with max value of int16, should pass and return the same value`` () =
    Native.Tests.testInt16 (Int16.MaxValue)
    |> should equal Int16.MaxValue

[<Test>]
let ``with max value of uint32, should pass and return the same value`` () =
    Native.Tests.testUInt32 (UInt32.MaxValue)
    |> should equal UInt32.MaxValue

[<Test>]
let ``with max value of int, should pass and return the same value`` () =
    Native.Tests.testInt32 (Int32.MaxValue)
    |> should equal Int32.MaxValue

[<Test>]
let ``with max value of uint64, should pass and return the same value`` () =
    Native.Tests.testUInt64 (UInt64.MaxValue)
    |> should equal UInt64.MaxValue

[<Test>]
let ``with max value of int64, should pass and return the same value`` () =
    Native.Tests.testInt64 (Int64.MaxValue)
    |> should equal Int64.MaxValue

[<Test>]
let ``with max value of single, should pass and return the same value`` () =
    Native.Tests.testSingle (Single.MaxValue)
    |> should equal Single.MaxValue

[<Test>]
let ``with max value of double, should pass and return the same value`` () =
    Native.Tests.testDouble (Double.MaxValue)
    |> should equal Double.MaxValue

[<Test>]
let ``with max value of byte, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testByte (Byte.MaxValue)
    |> should equal Byte.MaxValue

[<Test>]
let ``with max value of sbyte, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testSByte (SByte.MaxValue)
    |> should equal SByte.MaxValue

[<Test>]
let ``with max value of uint16, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testUInt16 (UInt16.MaxValue)
    |> should equal UInt16.MaxValue

[<Test>]
let ``with max value of int16, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testInt16 (Int16.MaxValue)
    |> should equal Int16.MaxValue

[<Test>]
let ``with max value of uint32, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testUInt32 (UInt32.MaxValue)
    |> should equal UInt32.MaxValue

[<Test>]
let ``with max value of int, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testInt32 (Int32.MaxValue)
    |> should equal Int32.MaxValue

[<Test>]
let ``with max value of uint64, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testUInt64 (UInt64.MaxValue)
    |> should equal UInt64.MaxValue

[<Test>]
let ``with max value of int64, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testInt64 (Int64.MaxValue)
    |> should equal Int64.MaxValue

[<Test>]
let ``with max value of single, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testSingle (Single.MaxValue)
    |> should equal Single.MaxValue

[<Test>]
let ``with max value of double, should pass and return the same value from exported`` () =
    GC.Collect (2)
    Native.Tests.testExported_testDouble (Double.MaxValue)
    |> should equal Double.MaxValue

[<Test>]
let ``with string, should pass string`` () =
    Native.Tests4.testString ("hello")

[<Test>]
let ``with byte array, should pass byte array and return first element`` () =
    Native.Tests4.testByteArray ([|255uy|])
    |> should equal 255uy
