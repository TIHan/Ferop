namespace Ferop

open System
open System.Runtime.CompilerServices

/// Marks a class to let Ferop know that its containing static methods marked with the
/// 'Import' attribute and an 'I' call will be compiled with a C/C++, then
/// the methods will be modified to be P/Invoke methods calling into the compiled code
/// that was in the 'C' call.
///
/// Static methods marked with 'Export' attribute will tell the C/C++ compiler
/// that the method needs to be called from within C/C++; this is handled
/// automatically.
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type FeropAttribute () =
    inherit Attribute ()

[<AttributeUsageAttribute (AttributeTargets.Class)>]
type ClangOsxAttribute (flags: string, libs: string) =
    inherit Attribute ()

[<AttributeUsageAttribute (AttributeTargets.Class)>]
type GccLinuxAttribute (flags: string, libs: string) =
    inherit Attribute ()

/// Allows a hook into the MSVC command line arguments when compiling
/// C/C++ on Windows.
///
/// Ferop will try to choose the very latest MSVC version based on what is stored in
/// 'HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\VisualStudio\SxS\Vs7' first, then
/// 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\VisualStudio\SxS\Vs7' last.
///
/// cl.exe {cfile} {options} /link /DLL /OUT:{dllName}
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type MsvcWinAttribute (options: string) =
    inherit Attribute ()

/// Marks a class to allow its imported static methods to contain C++ code.
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type CppAttribute () =
    inherit Attribute ()

/// Marks a class that contains the 'Ferop' attribute to inject the given
/// C/C++ code in the header file after the default header code.
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type HeaderAttribute (header: string) =
    inherit Attribute ()

/// Marks a class that contains the 'Ferop' attribute to inject the given
/// C/C++ code in the source file after the source include lines.
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type SourceAttribute (source: string) =
    inherit Attribute ()

/// Marks a static method that tells Ferop to compile code in C/C++
/// that is contained in the method. The method will be modified to
/// a P/Invoke method that will interop with the compiled code.
[<AttributeUsageAttribute (AttributeTargets.Method)>]
type ImportAttribute () =
    inherit Attribute ()

/// Marks a static method that tells Ferop that this method needs to be
/// accessible in C/C++. This is handled automatically.
[<AttributeUsageAttribute (AttributeTargets.Method)>]
type ExportAttribute () =
    inherit Attribute ()

[<AutoOpen>]
module Ferop =
    /// 'C' contains the code that will be compiled.
    /// Ferop will always remove this call when used in a static method marked with the 'Import' attribute
    /// that is inside a class marked with the 'Ferop' attribute.
    [<MethodImpl (MethodImplOptions.NoInlining)>]
    let C (code: string) = failwith "Ferop: The function has been inlined. Please mark it with [MethodImpl(MethodImplOptions.NoInlining)]"