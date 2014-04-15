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

[<Ferop>]
[<ClangFlagsOsx ("")>]
[<ClangLibsOsx ("")>]
[<MsvcLibsWin ("")>]
[<MsvcIncludesWin ("")>]
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

    let testStruct2 (x: Struct2) : Struct2 = C """ return x; """

    let testStruct3 (x: Struct3) : Struct3 = C """ return x; """

    [<Export>]
    let exported_testByte (x: byte) : byte = x

    let testExported_testByte (x: byte) : byte = C """ return Delegate_fs_Tests_exported_testByte (x); """
(*
    let testDelegate_testByte (x: byte) : byte =
        C """
return Fs_Tests_testByte (x);
"""

    let testDelegate_testSByte (x: sbyte) : sbyte =
        C """
return Fs_Tests_testSByte (x);
"""

    let testDelegate_testUInt16 (x: uint16) : uint16 =
        C """
return Fs_Tests_testUInt16 (x);
"""

    let testDelegate_testInt16 (x: int16) : int16 =
        C """
return Fs_Tests_testInt16 (x);
"""

    let testDelegate_testUInt32 (x: uint32) : uint32 =
        C """
return Fs_Tests_testUInt32 (x);
"""

    let testDelegate_testInt32 (x: int) : int =
        C """
return Fs_Tests_testInt32 (x);
"""

    let testDelegate_testUInt64 (x: uint64) : uint64 =
        C """
return Fs_Tests_testUInt64 (x);
"""

    let testDelegate_testInt64 (x: int64) : int64 =
        C """
return Fs_Tests_testInt64 (x);
"""

    let testDelegate_testSingle (x: single) : single =
        C """
return Fs_Tests_testSingle (x);
"""

    let testDelegate_testDouble (x: double) : double =
        C """
return Fs_Tests_testDouble (x);
"""
*)