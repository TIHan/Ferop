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
return app;
        """

    let exit (app: Application) : int =
        C """
SDL_GL_DeleteContext (app.GLContext);
SDL_DestroyWindow (app.Window);
SDL_Quit ();
return 0;
        """

    let clear () : unit = C """ glClear (GL_COLOR_BUFFER_BIT); """

    let draw (app: Application) : unit = C """ SDL_GL_SwapWindow (app.Window); """

    let shouldQuit () : bool =
        C """
SDL_Event e;
SDL_PollEvent (&e);
return e.type == SDL_QUIT;
        """

    let generateVbo (size: int) (data: nativeint) : int =
        C """
GLuint vbo;
glGenBuffers (1, &vbo);

glBindBuffer (GL_ARRAY_BUFFER, vbo);

glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
return vbo;
        """

    let drawVbo (size: int) (data: nativeint) (vbo: int) : unit =
        C """
glBindBuffer (GL_ARRAY_BUFFER, vbo);
glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
glDrawArrays (GL_LINES, 0, size);
        """

    let loadShaders (vertexSource: nativeint) (fragmentSource: nativeint) : unit =
        C """
GLuint vertexShader = glCreateShader (GL_VERTEX_SHADER);
glShaderSource (vertexShader, 1, &vertexSource, NULL);    
glCompileShader (vertexShader);

GLuint fragmentShader = glCreateShader (GL_FRAGMENT_SHADER);
glShaderSource (fragmentShader, 1, &fragmentSource, NULL);
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
