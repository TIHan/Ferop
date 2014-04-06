namespace Ferop.Tests

open Ferop.Code
open System.Runtime.InteropServices

#nowarn "9"

[<Struct>]
type Struct3 =
    val X : double
    val Y : double

[<Struct>]
type Struct2 =
    val X : Struct3
    val Y : int

[<Struct>]
type Struct1 =
    val X : single
    val Y : single
    val Z : single
    val W : nativeptr<single>
    val St2 : Struct2
    val St3 : Struct3

[<Ferop>]
[<ClangFlagsOsx ("")>]
[<ClangLibsOsx ("")>]
[<Include ("<stdio.h>")>]
module Tests =
    let testByte (x: byte) : byte = C """ return x; """

    let testSByte (x: sbyte) : sbyte = C """ return x; """

    let testUInt16 (x: uint16) : uint16 = C """ return x; """

    let testInt16 (x: int16) : int16 = C """ return x; """

    let testUInt32 (x: uint32) : uint32 = C """ return x; """

    let testInt32 (x: int) : int = C """ return x; """

    let testUInt64 (x: uint64) : uint64 = C """ return x; """

    let testInt64 (x: int64) : int64 = C """ return x; """

    let testSingle (x: single) : single = C """ return x; """

    let testDouble (x: double) : double = C """ return x; """

    let testStruct1 (x: Struct1) : Struct1 = C """ return x; """
