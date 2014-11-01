namespace Ferop.Sample

open FSharp.Interop.Ferop
open System.Runtime.InteropServices

#nowarn "9"

type InputEvent =
    | KeyPressed of char
    | KeyReleased of char

module InputInternal =
    let internal inputEvents = ResizeArray<InputEvent> ()

[<Struct>]
type Application =
    val Window : nativeint
    val GLContext : nativeint

[<Struct>]
type vec2 =
    val X : single
    val Y : single

    new (x, y) = { X = x; Y = y }

[<Struct>]
type DrawLine =
    val X : vec2
    val Y : vec2

    new (x, y) = { X = x; Y = y }

[<Struct>]
type KeyboardEvent =
    val IsPressed : int
    val KeyCode : int

[<Ferop>]
[<ClangFlagsOsx ("-DGL_GLEXT_PROTOTYPES -I/Library/Frameworks/SDL2.framework/Headers")>]
[<ClangLibsOsx ("-F/Library/Frameworks -framework Cocoa -framework OpenGL -framework IOKit -framework SDL2")>]
[<GccFlagsLinux ("-I../../include/SDL2")>]
[<GccLibsLinux ("-lSDL2")>]
#if __64BIT__
[<MsvcOptionsWin (""" /I ..\..\include\SDL2 /I ..\..\include ..\..\lib\win\x64\SDL2.lib ..\..\lib\win\x64\SDL2main.lib ..\..\lib\win\x64\glew32.lib opengl32.lib """)>]
[<Cpu64bit>]
#else
[<MsvcOptionsWin (""" /I ..\..\include\SDL2 /I ..\..\include ..\..\lib\win\x86\SDL2.lib ..\..\lib\win\x86\SDL2main.lib ..\..\lib\win\x86\glew32.lib opengl32.lib """)>]
#endif
[<Header ("""
#include <stdio.h>
#if defined(__GNUC__)
#   include "SDL.h"
#   include "SDL_opengl.h"
#else
#   include "SDL.h"
#   include <GL/glew.h>
#   include <GL/wglew.h>
#endif
""")>]
module App =

    [<Export>]
    let dispatchKeyboardEvent (kbEvt: KeyboardEvent) : unit =
        InputInternal.inputEvents.Add (
            if kbEvt.IsPressed = 1 then 
                InputEvent.KeyPressed (char kbEvt.KeyCode) 
            else 
                InputEvent.KeyReleased (char kbEvt.KeyCode))

    let initSystems () : unit =
        code """
SDL_Init (SDL_INIT_VIDEO);
"""

    let createWindow () : int =
        code """
        return SDL_CreateWindow(
            "Ferop.Sample",
            SDL_WINDOWPOS_UNDEFINED,
            SDL_WINDOWPOS_UNDEFINED,
            900, 900,
            SDL_WINDOW_OPENGL);
"""

    let createApp (window: int) : Application =
        code """
App_Application app;

app.Window = (SDL_Window*)window;

SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 2);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

app.GLContext = SDL_GL_CreateContext ((SDL_Window*)app.Window);
SDL_GL_SetSwapInterval (0);

#if defined(__GNUC__)
#else
glewExperimental = GL_TRUE;
glewInit ();
#endif

return app;
        """

    let exit (app: Application) : int =
        code """
SDL_GL_DeleteContext (app.GLContext);
SDL_DestroyWindow ((SDL_Window*)app.Window);
SDL_Quit ();
return 0;
        """

    let clear () : unit = code """ glClear (GL_COLOR_BUFFER_BIT); """

    let draw (app: Application) : unit = code """ SDL_GL_SwapWindow ((SDL_Window*)app.Window); """

    let pollInputEvents () : unit =
        code """
SDL_Event e;
while (SDL_PollEvent (&e))
{
    if (e.type == SDL_KEYDOWN)
    {
        SDL_KeyboardEvent* event = (SDL_KeyboardEvent*)&e;
        if (event->repeat != 0) continue;

        App_KeyboardEvent evt;
        evt.IsPressed = 1;
        evt.KeyCode = event->keysym.sym;

        App_dispatchKeyboardEvent (evt);
    }
    else if (e.type == SDL_KEYUP)
    {
        SDL_KeyboardEvent* event = (SDL_KeyboardEvent*)&e;
        if (event->repeat != 0) continue;

        App_KeyboardEvent evt;
        evt.IsPressed = 0;
        evt.KeyCode = event->keysym.sym;

        App_dispatchKeyboardEvent (evt);
    }
} 
        """

    let generateVbo (size: int) (data: DrawLine[]) : int =
        code """
GLuint vbo;
glGenBuffers (1, &vbo);

glBindBuffer (GL_ARRAY_BUFFER, vbo);

glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
return vbo;
        """

    let drawVbo (size: int) (data: DrawLine[]) (vbo: int) : unit =
        code """
glBindBuffer (GL_ARRAY_BUFFER, vbo);
glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
glDrawArrays (GL_LINES, 0, size);
        """

    let loadShaders (vertexSource: byte[]) (fragmentSource: byte[]) : unit =
        code """
GLuint vertexShader = glCreateShader (GL_VERTEX_SHADER);
glShaderSource (vertexShader, 1, (const GLchar*const*)&vertexSource, NULL);    
glCompileShader (vertexShader);

GLuint fragmentShader = glCreateShader (GL_FRAGMENT_SHADER);
glShaderSource (fragmentShader, 1, (const GLchar*const*)&fragmentSource, NULL);
glCompileShader (fragmentShader);

/******************************************************/

GLuint shaderProgram = glCreateProgram ();
glAttachShader (shaderProgram, vertexShader);
glAttachShader (shaderProgram, fragmentShader);

glBindFragDataLocation(shaderProgram, 0, "outColor");

glLinkProgram (shaderProgram);

glUseProgram (shaderProgram);

/******************************************************/

GLuint vao;
glGenVertexArrays (1, &vao);

glBindVertexArray (vao);

GLint posAttrib = glGetAttribLocation (shaderProgram, "position");

glVertexAttribPointer (posAttrib, 2, GL_FLOAT, GL_FALSE, 0, 0);

glEnableVertexAttribArray (posAttrib);
        """

module Input =
    let processInput () =
        let evts = InputInternal.inputEvents |> List.ofSeq
        InputInternal.inputEvents.Clear ()
        evts