namespace Ferop.Tests

open Ferop.Code
open System.Runtime.InteropServices

#nowarn "9"

[<Ferop>]
[<ClangFlagsOsx ("")>]
[<ClangLibsOsx ("")>]
[<Include ("<stdio.h>")>]
module Tests =
    let testByte (x: byte) : byte = C """ return x; """

    let testSbyte (x: sbyte) : sbyte = C """ return x; """