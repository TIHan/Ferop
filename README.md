Ferop
=

Ferop is a .NET library that allows inline C/C++ to compile and run on Windows/Linux/OSX.

Quick Start
-

F#
```fsharp
open System.Runtime.CompilerServices
open Ferop

[<Ferop>]
[<Header("""#include <stdio.h>""")>]
module Native =
    [<Import>]
    [<MethodImpl (MethodImplOptions.NoInlining)>]
    let printHelloWorld () : unit = C """printf("Hello World!\n");"""

[<EntryPoint>]
let main args =
    Native.printHelloWorld ()
    0
```

C#
```csharp
namespace Native
{
    using Ferop;

    [Ferop]
    [Header("#include <stdio.h>")]
    class Native
    {
        [Import]
        public static void PrintHelloWorld()
        {
            // 'object' is just a placeholder here for void return types.
            Ferop.C<object>("printf(\"Hello World!\\n\");");
        }

        static void Main(string[] args)
        {
            Native.PrintHelloWorld();
            System.Console.Read();
        }
    }
}
```
