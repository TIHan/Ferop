namespace Ferop.Tests.Native

open FSharp.Interop.Ferop
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

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

[<Struct>]
type Struct4 =
    val X : single
    val Y : single

type Enum1 =
    | Sub1 = 0
    | Sub2 = 1

type Enum2 =
    | Sub1 = 3
    | Sub2 = 1

#if __64BIT__
[<Cpu64bit>]
#else
#endif
[<Ferop>]
[<Header ("""
#include <stdio.h>
""")>]
type Tests =
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    static member testByte (x: byte) : byte = code """ return x; """
//
//    static member testTwoBytes (x: byte) (y: byte) : byte = code """ return x + y; """
//
//    static member testSByte (x: sbyte) : sbyte = code """ return x; """
//
//    static member testUInt16 (x: uint16) : uint16 = code """ return x; """
//
//    static member testInt16 (x: int16) : int16 = code """ return x; """
//
//    static member testUInt32 (x: uint32) : uint32 = code """ return x; """
//
//    static member testInt32 (x: int) : int = code """ return x; """
//
//    static member testUInt64 (x: uint64) : uint64 = code """ return x; """
//
//    static member testInt64 (x: int64) : int64 = code """ return x; """
//
//    static member testSingle (x: single) : single = code """ return x; """
//
//    static member testDouble (x: double) : double = code """ return x; """
//
//    static member testStruct1 (x: Struct1) : Struct1 = code """ return x; """
//
//    static member testStruct2 (x: Struct2) : Struct2 = code """ return x; """
//
//    static member testStruct3 (x: Struct3) : Struct3 = code """ return x; """
//
//    static member testStruct3Value (x: Struct3) : float = code """ return x.Y; """
//
//    [<Export>]
//    static member exported_testByte (x: byte) : byte = x
//
//    static member testExported_testByte (x: byte) : byte = code """ return Tests_exported_testByte (x); """
//
//    [<Export>]
//    static member exported_testTwoBytes (x: byte) (y: byte) : byte = x + y
//
//    static member testExported_testTwoBytes (x: byte) (y: byte) : byte = code """ return Tests_exported_testTwoBytes (x, y); """
//
//    [<Export>]
//    static member exported_testSByte (x: sbyte) : sbyte = x
//
//    static member testExported_testSByte (x: sbyte) : sbyte = code """ return Tests_exported_testSByte (x); """
//
//    [<Export>]
//    static member exported_testUInt16 (x: uint16) : uint16 = x
//
//    static member testExported_testUInt16 (x: uint16) : uint16 = code """ return Tests_exported_testUInt16 (x); """
//
//    [<Export>]
//    static member exported_testInt16 (x: int16) : int16 = x
//
//    static member testExported_testInt16 (x: int16) : int16 = code """ return Tests_exported_testInt16 (x); """
//
//    [<Export>]
//    static member exported_testUInt32 (x: uint32) : uint32 = x
//
//    static member testExported_testUInt32 (x: uint32) : uint32 = code """ return Tests_exported_testUInt32 (x); """
//
//    [<Export>]
//    static member exported_testInt32 (x: int) : int = x
//
//    static member testExported_testInt32 (x: int) : int = code """ return Tests_exported_testInt32 (x); """
//
//    [<Export>]
//    static member exported_testUInt64 (x: uint64) : uint64 = x
//
//    static member testExported_testUInt64 (x: uint64) : uint64 = code """ return Tests_exported_testUInt64 (x); """
//
//    [<Export>]
//    static member exported_testInt64 (x: int64) : int64 = x
//
//    static member testExported_testInt64 (x: int64) : int64 = code """ return Tests_exported_testInt64 (x); """
//
//    [<Export>]
//    static member exported_testSingle (x: single) : single = x
//
//    static member testExported_testSingle (x: single) : single = code """ return Tests_exported_testSingle (x); """
//
//    [<Export>]
//    static member exported_testDouble (x: double) : double = x
//
//    static member testExported_testDouble (x: double) : double = code """ return Tests_exported_testDouble (x); """
//
//#if __64BIT__
//[<Cpu64bit>]
//#else
//#endif
//[<Ferop>]
//module Tests2 =
//    let testByte (x: byte) : byte = code """ return x; """
//
//#if __64BIT__
//[<Cpu64bit>]
//#else
//#endif
//[<Ferop>]
//module Tests3 =
//    let testEnum1 (x: Enum1) : Enum1 = code """ return x; """
//
//    let testEnum2 (x: Enum2) : Enum2 = code """ return x; """
//
//#if __64BIT__
//[<Cpu64bit>]
//#else
//#endif
//[<Ferop>]
//module Tests4 =
//    [<UnmanagedFunctionPointerAttribute (CallingConvention.Cdecl)>]
//    type Test = delegate of int -> int
//
//    let testByteArray (x: byte[]) : byte = code """ return x[0]; """
//
//    let testDelegate (f: Test) : int = code """ return f (1234); """
//
//    let testRecursiveStruct (x: RecursiveStruct40) : unit = code """ """
//
//    let testStructPointer (xp: nativeptr<RecursiveStruct40>) : unit = code """ """
//
//    let testByRef (xbr: double byref) : unit = code """ *xbr = 30.2; """
//
//    let testPointer (xp: nativeptr<double>) : unit = code """ *xp = 36.2; """
//
//    let testArray (xs: double []) : unit = code """ xs[2] = 20.45; """
//
//    let testStructPointer2 (xp: nativeptr<Struct4>) : unit = code """ """
//
//    let testNativeInt (x: nativeint) : unit = code """ """
//
//    let testUnativeInt (x: unativeint) : unit = code """ """
//
//#if __64BIT__
//[<Cpu64bit>]
//#else
//#endif
//[<Ferop>]
//[<Cpp>]
//[<Header ("""
//#include <iostream>
//using namespace std;
//""")>]
//module TestsCpp =
//    let testCppHelloWorld () : unit = code """cout << "Hello World!\n";"""