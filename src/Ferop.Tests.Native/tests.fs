namespace Ferop.Tests

open FSharp.Interop.Ferop
open System.Runtime.InteropServices

#nowarn "9"

type [<Struct>] RecursiveStruct50 =
    val X : int

type [<Struct>] RecursiveStruct49 =
    val X : int
    val Y : RecursiveStruct50

type [<Struct>] RecursiveStruct48 =
    val X : int
    val Y : RecursiveStruct49

type [<Struct>] RecursiveStruct47 =
    val X : int
    val Y : RecursiveStruct48

type [<Struct>] RecursiveStruct46 =
    val X : int
    val Y : RecursiveStruct47

type [<Struct>] RecursiveStruct45 =
    val X : int
    val Y : RecursiveStruct46

type [<Struct>] RecursiveStruct44 =
    val X : int
    val Y : RecursiveStruct45

type [<Struct>] RecursiveStruct43 =
    val X : int
    val Y : RecursiveStruct44

type [<Struct>] RecursiveStruct42 =
    val X : int
    val Y : RecursiveStruct43

type [<Struct>] RecursiveStruct41 =
    val X : int
    val Y : RecursiveStruct42

type [<Struct>] RecursiveStruct40 =
    val X : int
    val Y : RecursiveStruct41

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

        new (x, y) = { X = x; Y = y }

type Enum1 =
    | Sub1 = 0
    | Sub2 = 1

type Enum2 =
    | Sub1 = 3
    | Sub2 = 1

[<ReflectedDefinition>]
[<Header ("""
#include <stdio.h>
""")>]
type Tests =
    static member testByte (x: byte) : byte = C """ return x; """

    static member testTwoBytes (x: byte) (y: byte) : byte = C """ return x + y; """

    static member testSByte (x: sbyte) : sbyte = C """ return x; """

    static member testUInt16 (x: uint16) : uint16 = C """ return x; """

    static member testInt16 (x: int16) : int16 = C """ return x; """

    static member testUInt32 (x: uint32) : uint32 = C """ return x; """

    static member testInt32 (x: int) : int = C """ return x; """

    static member testUInt64 (x: uint64) : uint64 = C """ return x; """

    static member testInt64 (x: int64) : int64 = C """ return x; """

    static member testSingle (x: single) : single = C """ return x; """

    static member testDouble (x: double) : double = C """ return x; """

    static member testStruct1 (x: Struct1) : Struct1 = C """ return x; """

    static member testStruct2 (x: Struct2) : Struct2 = C """ return x; """

    static member testStruct3 (x: Struct3) : Struct3 = C """ return x; """

    static member testStruct3Value (x: Struct3) : float = C """ return x.Y; """

    [<Export>]
    static member exported_testByte (x: byte) : byte = x

    static member testExported_testByte (x: byte) : byte = C """ return Tests_exported_testByte (x); """

    [<Export>]
    static member exported_testTwoBytes (x: byte) (y: byte) : byte = x + y

    static member testExported_testTwoBytes (x: byte) (y: byte) : byte = C """ return Tests_exported_testTwoBytes (x, y); """

    [<Export>]
    static member exported_testSByte (x: sbyte) : sbyte = x

    static member testExported_testSByte (x: sbyte) : sbyte = C """ return Tests_exported_testSByte (x); """

    [<Export>]
    static member exported_testUInt16 (x: uint16) : uint16 = x

    static member testExported_testUInt16 (x: uint16) : uint16 = C """ return Tests_exported_testUInt16 (x); """

    [<Export>]
    static member exported_testInt16 (x: int16) : int16 = x

    static member testExported_testInt16 (x: int16) : int16 = C """ return Tests_exported_testInt16 (x); """

    [<Export>]
    static member exported_testUInt32 (x: uint32) : uint32 = x

    static member testExported_testUInt32 (x: uint32) : uint32 = C """ return Tests_exported_testUInt32 (x); """

    [<Export>]
    static member exported_testInt32 (x: int) : int = x

    static member testExported_testInt32 (x: int) : int = C """ return Tests_exported_testInt32 (x); """

    [<Export>]
    static member exported_testUInt64 (x: uint64) : uint64 = x

    static member testExported_testUInt64 (x: uint64) : uint64 = C """ return Tests_exported_testUInt64 (x); """

    [<Export>]
    static member exported_testInt64 (x: int64) : int64 = x

    static member testExported_testInt64 (x: int64) : int64 = C """ return Tests_exported_testInt64 (x); """

    [<Export>]
    static member exported_testSingle (x: single) : single = x

    static member testExported_testSingle (x: single) : single = C """ return Tests_exported_testSingle (x); """

    [<Export>]
    static member exported_testDouble (x: double) : double = x

    static member testExported_testDouble (x: double) : double = C """ return Tests_exported_testDouble (x); """

[<ReflectedDefinition>]
module Tests2 =
    let testByte (x: byte) : byte = C """ return x; """

[<ReflectedDefinition>]
module Tests3 =
    let testEnum1 (x: Enum1) : Enum1 = C """ return x; """

    let testEnum2 (x: Enum2) : Enum2 = C """ return x; """

[<ReflectedDefinition>]
module Tests4 =
    [<UnmanagedFunctionPointerAttribute (CallingConvention.Cdecl)>]
    type TestDelegate = delegate of int -> int

    let testByteArray (x: byte[]) : byte = C """ return x[0]; """

    let testDelegate (f: TestDelegate) : int = C """ return f (1234); """

    let testRecursiveStruct (x: RecursiveStruct40) : unit = C """ """

[<ReflectedDefinition>]
[<Cpp>]
[<Header ("""
#include <iostream>
""")>]
module TestsCpp =
    let testCppHelloWorld () : unit = C """std::cout << "Hello World!\n";"""