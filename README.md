Ferop
=

Ferop is a .NET library that allows inline C/C++ to compile and run on Windows/Linux/OSX.

iOS and Android support is planned.

Quick Start
-

<h3>F#</h3>
```fsharp
open Ferop

[<Ferop>]
[<Header("""#include <stdio.h>""")>]
module Native =
    [<Import; MI(MIO.NoInlining)>]
    let printHelloWorld() : unit = C """printf("Hello World!\n");"""

[<EntryPoint>]
let main args =
    Native.printHelloWorld()
    0
```

<h3>C#</h3>
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
            Ferop.C (@"printf(""Hello World!\n"");");
        }

        static void Main(string[] args)
        {
            PrintHelloWorld();
            System.Console.Read();
        }
    }
}
```

<h3>VB</h3>
```vb
Imports Ferop

<Ferop>
<Header("#include <stdio.h>")>
Module Native

    <Import>
    Public Sub PrintHelloWorld()
        Call Ferop.C("printf(""Hello World!\n"");")
    End Sub

    Sub Main()
        Call PrintHelloWorld()
        Console.ReadLine()
    End Sub

End Module
```
