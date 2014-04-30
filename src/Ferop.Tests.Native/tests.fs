namespace Ferop.Tests

open Ferop.Code
open System.Runtime.InteropServices

#nowarn "9"

[<Struct>]
type Struct2 =
    val X : Struct3
    val Y : int

and [<Struct>]
    Struct1 =
    val X : single
    val Y : single
    val Z : single
    val W : nativeptr<single>
    val St2 : Struct2
    val St3 : Struct3

and [<Struct>]
    Struct3 =
    val X : double
    val Y : double

type Enum1 =
    | Sub1 = 0
    | Sub2 = 1

type Enum2 =
    | Sub1 = 3
    | Sub2 = 1

[<Ferop>]
[<ClangFlagsOsx ("")>]
[<ClangLibsOsx ("")>]
[<MsvcLibsWin ("")>]
[<MsvcIncludesWin ("")>]
[<Include ("<stdio.h>")>]
module Tests =
    let testByte (x: byte) : byte = C """ return x; """

    let testTwoBytes (x: byte) (y: byte) : byte = C """ return x + y; """

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

    let testStruct2 (x: Struct2) : Struct2 = C """ return x; """

    let testStruct3 (x: Struct3) : Struct3 = C """ return x; """

    [<Export>]
    let exported_testByte (x: byte) : byte = x

    let testExported_testByte (x: byte) : byte = C """ return Tests_exported_testByte (x); """

    [<Export>]
    let exported_testTwoBytes (x: byte) (y: byte) : byte = x + y

    let testExported_testTwoBytes (x: byte) (y: byte) : byte = C """ return Tests_exported_testTwoBytes (x, y); """

    [<Export>]
    let exported_testSByte (x: sbyte) : sbyte = x

    let testExported_testSByte (x: sbyte) : sbyte = C """ return Tests_exported_testSByte (x); """

    [<Export>]
    let exported_testUInt16 (x: uint16) : uint16 = x

    let testExported_testUInt16 (x: uint16) : uint16 = C """ return Tests_exported_testUInt16 (x); """

    [<Export>]
    let exported_testInt16 (x: int16) : int16 = x

    let testExported_testInt16 (x: int16) : int16 = C """ return Tests_exported_testInt16 (x); """

    [<Export>]
    let exported_testUInt32 (x: uint32) : uint32 = x

    let testExported_testUInt32 (x: uint32) : uint32 = C """ return Tests_exported_testUInt32 (x); """

    [<Export>]
    let exported_testInt32 (x: int) : int = x

    let testExported_testInt32 (x: int) : int = C """ return Tests_exported_testInt32 (x); """

    [<Export>]
    let exported_testUInt64 (x: uint64) : uint64 = x

    let testExported_testUInt64 (x: uint64) : uint64 = C """ return Tests_exported_testUInt64 (x); """

    [<Export>]
    let exported_testInt64 (x: int64) : int64 = x

    let testExported_testInt64 (x: int64) : int64 = C """ return Tests_exported_testInt64 (x); """

    [<Export>]
    let exported_testSingle (x: single) : single = x

    let testExported_testSingle (x: single) : single = C """ return Tests_exported_testSingle (x); """

    [<Export>]
    let exported_testDouble (x: double) : double = x

    let testExported_testDouble (x: double) : double = C """ return Tests_exported_testDouble (x); """

[<Ferop>]
[<ClangFlagsOsx ("")>]
[<ClangLibsOsx ("")>]
[<MsvcLibsWin ("")>]
[<MsvcIncludesWin ("")>]
module Tests2 =
    let testByte (x: byte) : byte = C """ return x; """

[<Ferop>]
[<ClangFlagsOsx ("")>]
[<ClangLibsOsx ("")>]
[<MsvcLibsWin ("")>]
[<MsvcIncludesWin ("")>]
module Tests3 =
    let testEnum1 (x: Enum1) : Enum1 = C """ return x; """

    let testEnum2 (x: Enum2) : Enum2 = C """ return x; """