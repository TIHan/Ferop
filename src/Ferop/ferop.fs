namespace Ferop

open System
open System.Runtime.CompilerServices

/// Marks a class to let Ferop know that its containing static methods marked with the
/// 'Import' attribute and a 'C' call, the methods will be modified to be P/Invoke methods,
/// and the text of the 'C' call will compile as C/C++ code. The P/Invoke methods will know how to
/// call into the compiled C/C++ code.
///
/// Static methods marked with 'Export' attribute will tell the C/C++ compiler
/// that the method needs to be called from within C/C++; this is handled
/// automatically.
///
/// Ferop will be able to determine to compile 32-bit or 64-bit C/C++ based on what
/// architecture is defined by the assembly.
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type FeropAttribute () =
    inherit Attribute ()

/// Marks a class to allow a hook into the clang command line arguments when compiling
/// C/C++ on OSX. The two hooks are {flags} and {libs}.
///
/// The compiler must have access to <stdint.h>.
///
/// 32-bit C:   
///     clang -Wall -arch i386 {flags} -c {cFile} -o {oFile}
/// 32-bit C++: 
///     clang -Wall -arch i386 {flags} -c {cFile} -o {oFile}
/// 32-bit Dynamic Library: 
///     clang -arch i386 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 {libs} {oFile} -o {dylibName}
/// 64-bit C:   
///     clang -Wall -arch x86_64 {flags} -c {cFile} -o {oFile}
/// 64-bit C++: 
///     clang -Wall -arch x86_64 {flags} -c {cFile} -o {oFile}
/// 64-bit Dynamic Library: 
///     clang -arch x86_64 -dynamiclib -headerpad_max_install_names -undefined dynamic_lookup -compatibility_version 1.0 -current_version 1.0 {libs} {oFile} -o {dylibName}
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type ClangOsxAttribute (flags: string, libs: string) =
    inherit Attribute ()

[<AttributeUsageAttribute (AttributeTargets.Class)>]
type ClangiOSAttribute (flags: string) =
    inherit Attribute ()

/// Marks a class to allow a hook into the gcc/g++ command line arguments when compiling
/// C/C++ on Linux. The two hooks are {flags} and {libs}.
///
/// The compiler must have access to <stdint.h>.
/// 
/// 32-bit C:
///     gcc -Wall -m32 -fPIC {flags} -c {cFile} -o {oFile}
/// 32-bit C++:
///     g++ -Wall -m32 -fPIC {flags} -c {cFile} -o {oFile}
/// 32-bit Dynamic Library:
///     gcc -m32 {oFile} -shared -o {soName} {libs}
/// 64-bit C:
///     gcc -Wall -m64 -fPIC {flags} -c {cFile} -o {oFile}
/// 64-bit C++:
///     g++ -Wall -m64 -fPIC {flags} -c {cFile} -o {oFile}
/// 64-bit Dynamic Library:
///     g++ -m64 {oFile} -shared -o {soName} {libs}
[<AttributeUsageAttribute (AttributeTargets.Class)>]
type GccLinuxAttribute (flags: string, libs: string) =
    inherit Attribute ()

/// Marks a class to allow a hook into the MSVC command line arguments when compiling
/// C/C++ on Windows. The hook is {options}.
///
/// The compiler must have access to <stdint.h>.
///
/// Ferop will try to choose the very latest MSVC version based on what is stored in
/// 'HKEY_LOCAL_MACHINE\SOFTWARE\Wow6432Node\Microsoft\VisualStudio\SxS\Vs7' first, then
/// 'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\VisualStudio\SxS\Vs7' last.
///
/// 32-bit / 64-bit C/C++:
///     cl.exe {cFile} {options} /link /DLL /OUT:{dllName}
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

type MIAttribute = MethodImplAttribute
type MIO = MethodImplOptions

[<AutoOpen>]
module FeropModule =
    let internal errorMsg = "Ferop: The function was not imported or has been inlined. If inlined, please mark it with [MethodImpl(MethodImplOptions.NoInlining)]"

    /// 'C' contains the code that will be compiled.
    /// Ferop will always remove this call when used in a static method marked with the 'Import' attribute
    /// that is inside a class marked with the 'Ferop' attribute.
    [<MI (MIO.NoInlining)>]
    let C (code: string) = failwith errorMsg

type Ferop private () =
    /// 'C' contains the code that will be compiled.
    /// Ferop will always remove this call when used in a static method marked with the 'Import' attribute
    /// that is inside a class marked with the 'Ferop' attribute.
    [<MI (MIO.NoInlining)>]
    static member C (code: string) = failwith errorMsg

    /// 'C' contains the code that will be compiled.
    /// Ferop will always remove this call when used in a static method marked with the 'Import' attribute
    /// that is inside a class marked with the 'Ferop' attribute.
    [<MI (MIO.NoInlining)>]
    static member C (code: string) : unit = failwith errorMsg