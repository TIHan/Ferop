namespace Ferop.Sample

open FSharp.Interop.Ferop
open System.Runtime.InteropServices

#nowarn "9"

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

[<ReflectedDefinition>]
[<ClangFlagsOsx ("-DGL_GLEXT_PROTOTYPES")>]
[<ClangLibsOsx ("-framework Cocoa -framework OpenGL -framework IOKit -framework SDL2")>]
#if __64BIT__
[<MsvcOptionsWin (""" /I ..\..\include ..\..\lib\win\x64\SDL2.lib ..\..\lib\win\x64\SDL2main.lib ..\..\lib\win\x64\glew32.lib opengl32.lib """)>]
[<Msvc64bit>]
#else
[<MsvcOptionsWin (""" /I ..\..\include ..\..\lib\win\x86\SDL2.lib ..\..\lib\win\x86\SDL2main.lib ..\..\lib\win\x86\glew32.lib opengl32.lib """)>]
#endif
[<Header ("""
#include <stdio.h>
#include <SDL2/SDL.h>
#include <GL/glew.h>
#include <GL/wglew.h>
""")>]
module App =
    let init () : Application =
        code """
SDL_Init (SDL_INIT_VIDEO);

App_Application app;

app.Window = 
    SDL_CreateWindow(
        "Ferop.Sample",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        900, 900,
        SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE);

SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 2);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

app.GLContext = SDL_GL_CreateContext ((SDL_Window*)app.Window);

glewExperimental = GL_TRUE;
glewInit ();

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

    let shouldQuit () : int =
        code """
SDL_Event e;
SDL_PollEvent (&e);
return e.type == SDL_QUIT;
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
