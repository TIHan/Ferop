module Ferop.Sample.Main

open System
open System.IO
open System.Diagnostics
open System.Runtime

open Ferop

type InputEvent =
    | KeyPressed of char
    | KeyReleased of char

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
   
type SoundChannel = SoundChannel of nativeint

[<Ferop>]
[<ClangOsx (
    "-DGL_GLEXT_PROTOTYPES -I/Library/Frameworks/SDL2.framework/Headers -I../../include -rpath .",
    "-F/Library/Frameworks -framework Cocoa -framework OpenGL -framework IOKit -framework SDL2 -rpath . -L../../lib -lfmod"
)>]
[<GccLinux ("-I../../include/SDL2", "-lSDL2")>]
#if __64BIT__
[<MsvcWin (""" /I ..\..\include\SDL2 /I ..\..\include ..\..\lib\win\x64\SDL2.lib ..\..\lib\win\x64\SDL2main.lib ..\..\lib\win\x64\glew32.lib opengl32.lib """)>]
#else
[<MsvcWin (""" /I ..\..\include\SDL2 /I ..\..\include ..\..\lib\win\x86\SDL2.lib ..\..\lib\win\x86\SDL2main.lib ..\..\lib\win\x86\glew32.lib opengl32.lib """)>]
#endif
[<Header ("""
#include <stdio.h>
#include <fmod.hpp>
#if defined(__GNUC__)
#   include "SDL.h"
#   include "SDL_opengl.h"
#else
#   include "SDL.h"
#   include <GL/glew.h>
#   include <GL/wglew.h>
#endif
""")>]
[<Cpp>]
module App =

    let inputEvents = ResizeArray<InputEvent> ()

    [<Export>]
    let dispatchKeyboardEvent (kbEvt: KeyboardEvent) : unit =
        inputEvents.Add (
            if kbEvt.IsPressed = 1 then 
                InputEvent.KeyPressed (char kbEvt.KeyCode) 
            else 
                InputEvent.KeyReleased (char kbEvt.KeyCode))

    [<Import; MI (MIO.NoInlining)>]
    let _createSoundChannel () : nativeint =
        C """
    FMOD::System     *system;
    FMOD::Sound      *sound1, *sound2, *sound3;
    FMOD::Channel    *channel = 0;
    FMOD::DSP        *dsp;
    FMOD_RESULT       result;
    unsigned int      version;
    void             *extradriverdata = 0;


    result = FMOD::System_Create(&system);

    result = system->getVersion(&version);

    result = system->init(32, FMOD_INIT_NORMAL, extradriverdata);

    result = system->createDSPByType(FMOD_DSP_TYPE_FFT, &dsp);

    result = system->createSound("sound.mp3", FMOD_DEFAULT, 0, &sound1);
    system->playSound(sound1, 0, false, &channel);
    channel->addDSP(0, dsp);

    return channel;
        """

    [<Import; MI (MIO.NoInlining)>]
    let _getSoundChannelVolume (p: nativeint) : single =
        C """
        FMOD::Channel *channel = (FMOD::Channel*)p;
        FMOD::DSP *dsp;
        float volume;

        channel->getDSP(0, &dsp);
   
        FMOD_DSP_PARAMETER_FFT *fft;
        dsp->getParameterData(FMOD_DSP_FFT_SPECTRUMDATA, (void **)&fft, 0, 0, 0);

        return *fft->spectrum[0];
        """

    let createSoundChannel () : SoundChannel = SoundChannel (_createSoundChannel ())

    let getSoundChannelVolume (ch: SoundChannel) =
        match ch with
        | SoundChannel p -> _getSoundChannelVolume p

    [<Import; MI (MIO.NoInlining)>]
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
        SDL_WINDOW_OPENGL);

SDL_GL_SetAttribute (SDL_GL_CONTEXT_MAJOR_VERSION, 3);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_MINOR_VERSION, 2);
SDL_GL_SetAttribute (SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);

app.GLContext = SDL_GL_CreateContext ((SDL_Window*)app.Window);
SDL_GL_SetSwapInterval (2);

#if defined(__GNUC__)
#else
glewExperimental = GL_TRUE;
glewInit ();
#endif

return app;
        """

    [<Import; MI (MIO.NoInlining)>]
    let exit (app: Application) : int =
        C """
SDL_GL_DeleteContext (app.GLContext);
SDL_DestroyWindow ((SDL_Window*)app.Window);
SDL_Quit ();
return 0;
        """
    
    [<Import; MI (MIO.NoInlining)>]
    let clear () : unit = C """ glClear (GL_COLOR_BUFFER_BIT); """

    [<Import; MI (MIO.NoInlining)>]
    let draw (app: Application) : unit = C """ SDL_GL_SwapWindow ((SDL_Window*)app.Window); """

    [<Import; MI (MIO.NoInlining)>]
    let pollInputEvents () : unit =
        C """
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

    [<Import; MI (MIO.NoInlining)>]
    let generateVbo (size: int) (data: DrawLine[]) : int =
        C """
GLuint vbo;
glGenBuffers (1, &vbo);

glBindBuffer (GL_ARRAY_BUFFER, vbo);

glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
return vbo;
        """

    [<Import; MI (MIO.NoInlining)>]
    let drawVbo (size: int) (data: DrawLine[]) (vbo: int) : unit =
        C """
glBindBuffer (GL_ARRAY_BUFFER, vbo);
glBufferData (GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW);
glDrawArrays (GL_LINES, 0, size);
        """

    [<Import; MI (MIO.NoInlining)>]
    let loadShaders (vertexSource: byte[]) (fragmentSource: byte[]) : unit =
        C """
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

    [<Import; MI (MIO.NoInlining)>]
    let setUniformColor (uniformColor: int) (r: single) (g: single) (b: single) (a: single) : unit =
        C """
glUniform4f (uniformColor, r, g, b, a);
        """

module Input =
    let processInput () =
        let evts = App.inputEvents |> List.ofSeq
        App.inputEvents.Clear ()
        evts

let clear () = App.clear ()

let draw app = App.draw app

let loadShaders () =
    let mutable vertexFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("v.vertex"))
    let mutable fragmentFile = ([|0uy|]) |> Array.append (File.ReadAllBytes ("f.fragment"))

    App.loadShaders vertexFile fragmentFile

let makeVbo (drawLines: DrawLine []) = 
    let vbo = App.generateVbo (drawLines.Length * sizeof<DrawLine>) drawLines
    vbo

let drawVbo (drawLines: DrawLine []) vbo =
    App.drawVbo (drawLines.Length * sizeof<DrawLine>) drawLines vbo

let init () = 
    GCSettings.LatencyMode <- GCLatencyMode.Batch
    App.init ()

let exit app = App.exit app

let pollInputEvents () = App.pollInputEvents ()

let torad = 0.0174532925f

let lrad = 20.f * torad
let rrad = -lrad

let inline makeEndpoint rads length (v: vec2) = vec2 (v.X + length * cos rads, v.Y + length * sin rads)

let inline makeDrawLine rads length (line: DrawLine) = DrawLine (line.Y, makeEndpoint rads length line.Y)

let makeLines degrees length (line: DrawLine) =

    let rec makeLines rads length (lines: DrawLine list) cont = function
        | 11 -> cont lines
        | n ->
            let ldeg = rads + lrad
            let rdeg = rads + rrad
            let ll = makeDrawLine ldeg length lines.Head
            let rl = makeDrawLine rdeg length lines.Head
            let n = n + 1
            let length = length * 0.7f
      
            makeLines ldeg length (ll :: lines) (fun x ->
                makeLines rdeg length (rl :: x) cont n) n

    makeLines (degrees * torad) length [line] (fun x -> x) 0

// http://gafferongames.com/game-physics/fix-your-timestep/
module GameLoop =
    type private GameLoop<'T> = { 
        State: 'T
        PreviousState: 'T
        LastTime: int64
        UpdateTime: int64
        UpdateAccumulator: int64
        RenderAccumulator: int64
        RenderFrameCount: int
        RenderFrameCountTime: int64
        RenderFrameLastCount: int }

    let start (state: 'T) (pre: unit -> unit) (update: int64 -> int64 -> 'T -> 'T) (render: float32 -> 'T -> 'T -> unit) =
        let targetUpdateInterval = (1000. / 30.) * 10000. |> int64
        let targetRenderInterval = (1000. / 120.) * 10000. |> int64
        let skip = (1000. / 5.) * 10000. |> int64

        let stopwatch = Stopwatch.StartNew ()
        let inline time () = stopwatch.Elapsed.Ticks

        let rec loop gl =
            let currentTime = time ()
            let deltaTime =
                match currentTime - gl.LastTime with
                | x when x > skip -> skip
                | x -> x

            let updateAcc = gl.UpdateAccumulator + deltaTime

            // We do not want our render accumulator going out of control,
            // so let's put a limit of its interval.
            let renderAcc = 
                match gl.RenderAccumulator with
                | x when x > targetRenderInterval -> targetRenderInterval
                | x -> x + deltaTime

            let rec processUpdate gl =
                if gl.UpdateAccumulator >= targetUpdateInterval
                then
                    let state = update gl.UpdateTime targetUpdateInterval gl.State

                    processUpdate
                        { gl with 
                            State = state
                            PreviousState = gl.State
                            UpdateTime = gl.UpdateTime + targetUpdateInterval
                            UpdateAccumulator = gl.UpdateAccumulator - targetUpdateInterval }
                else
                    gl

            let processRender gl =
                if gl.RenderAccumulator >= targetRenderInterval then
                    render (single gl.UpdateAccumulator / single targetUpdateInterval) gl.PreviousState gl.State

                    let renderCount, renderCountTime, renderLastCount =
                        if currentTime >= gl.RenderFrameCountTime + (10000L * 1000L) then
                            printfn "%A" gl.RenderFrameLastCount
                            1, gl.RenderFrameCountTime + (10000L * 1000L), gl.RenderFrameCount
                        else
                            gl.RenderFrameCount + 1, gl.RenderFrameCountTime, gl.RenderFrameLastCount

                    { gl with 
                        LastTime = currentTime
                        RenderAccumulator = gl.RenderAccumulator - targetRenderInterval
                        RenderFrameCount = renderCount
                        RenderFrameCountTime = renderCountTime
                        RenderFrameLastCount = renderLastCount }
                else
                    { gl with LastTime = currentTime }

            pre ()
       
            { gl with UpdateAccumulator = updateAcc; RenderAccumulator = renderAcc }
            |> processUpdate
            |> processRender
            |> loop

        loop
            { State = state
              PreviousState = state
              LastTime = 0L
              UpdateTime = 0L
              UpdateAccumulator = targetUpdateInterval
              RenderAccumulator = 0L
              RenderFrameCount = 0
              RenderFrameCountTime = 0L
              RenderFrameLastCount = 0 }

[<EntryPoint>]
let main args =
    let ch = App.createSoundChannel ()
    let app = init ()

    let beginPoint = vec2 (0.f, -1.f)
    let endPoint = vec2 (0.f, -0.5f)
    let drawLine = DrawLine (beginPoint, endPoint)

    let vbo = makeVbo [||]

    loadShaders ()

    let inline lerp x y t = x + (y - x) * t

    let refLength = ref 0.4f
    let refIsUpPressed = ref false
    let refIsDownPressed = ref false

    GameLoop.start ([||], 0.f) 
        (fun () ->
            pollInputEvents ())
            // update 30 fps
        (fun _ time (_, previousLength) ->
            GC.Collect ()
            let length = App.getSoundChannelVolume (ch) * 50.f

            let length =
                if length > previousLength + 0.02f then
                    previousLength + 0.02f
                elif length < previousLength - 0.02f then
                    previousLength - 0.02f
                else
                    length

            makeLines 90.f length drawLine
            |> Array.ofList, length) 
            // render fast as possible
        (fun t (prevDrawLines,prev) (drawLines,length) ->
            let t = single t

            let lerpedDrawLines =
                if prevDrawLines.Length <> drawLines.Length
                then prevDrawLines
                else
                    (prevDrawLines, drawLines)
                    ||> Array.map2 (fun prev x ->
                        DrawLine (
                            vec2 (lerp prev.X.X x.X.X t, lerp prev.X.Y x.X.Y t),
                            vec2 (lerp prev.Y.X x.Y.X t, lerp prev.Y.Y x.Y.Y t)))

            clear ()
            let l = length + prev
            App.setUniformColor 0 (1.f - l * 2.f) (length) (1.f - prev * 2.f) 1.0f
            drawVbo lerpedDrawLines vbo
            draw app)

    exit (app)