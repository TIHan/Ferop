namespace Ferop.Sample

open Ferop.Code
open System.Runtime.InteropServices

#nowarn "9"

[<Struct>]
type Application =
    val Window : nativeint
    val GLContext : nativeint

[<Ferop>]
[<ClangFlagsOsx ("-DGL_GLEXT_PROTOTYPES")>]
[<ClangLibsOsx ("-framework Cocoa -framework OpenGL -framework IOKit -framework SDL2")>]
[<Include ("<stdio.h>")>]
[<Include ("<SDL2/SDL.h>")>]
[<Include ("<SDL2/SDL_opengl.h>")>]
module App =
    let init () : Application =
        C """
SDL_Init (SDL_INIT_VIDEO);

Application app;

app.Window = 
    (int32_t*)SDL_CreateWindow(
        "Ferop.Sample",
        SDL_WINDOWPOS_UNDEFINED,
        SDL_WINDOWPOS_UNDEFINED,
        900, 900,
        SDL_WINDOW_OPENGL|SDL_WINDOW_RESIZABLE);

SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 2);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

app.GLContext = (int32_t*)SDL_GL_CreateContext ((SDL_Window*)app.Window);
return app;
        """

    let exit (app: Application) : int =
        C """
SDL_GL_DeleteContext ((SDL_Window*)app.GLContext);
SDL_DestroyWindow ((SDL_GLContext*)app.Window);
SDL_Quit ();
return 0;
        """
(*
    let generateVBO (size: int) (data: byte []) : int =
        C """
GLuint vbo;
glGenBuffers (1, &vbo);

glBindBuffer (GL_ARRAY_BUFFER, vbo);

glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
return vbo;
        """

    let loadShaders (vertexSource: sbyte []) (fragmentSource: sbyte []) : unit =
        C """
GLuint vertexShader = glCreateShader (GL_VERTEX_SHADER);
glShaderSource (vertexShader, 1, vertexSource, NULL);    
glCompileShader (vertexShader);

GLuint fragmentShader = glCreateShader (GL_FRAGMENT_SHADER);
glShaderSource (fragmentShader, 1, fragmentSource, NULL);
glCompileShader (fragmentShader);
        """
*)