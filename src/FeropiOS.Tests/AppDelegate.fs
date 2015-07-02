namespace FeropiOS.Tests

open System
open UIKit
open Foundation
open Ferop

open System.Runtime.InteropServices

[<Struct>]
type TestMe =
    val Y : int
    val X : int
    val wut : nativeint

//[<Ferop; Cpp>]
//[<ClangiOS ("""-I/Users/willsmith/Wwise/Wwise_v2014.1.5_build_5282/SDK/include""")>]
//[<Source ("""
//#include <cstdlib>
//namespace AK
//{
//  void * AllocHook( size_t in_size )
//  {
//    return malloc( in_size );
//  }
//  void FreeHook( void * in_ptr )
//  {
//    free( in_ptr );
//  }
//}
//
//#include <AK/SoundEngine/Platforms/iOS/AkiOSSoundEngine.h>
//#include <AK/SoundEngine/Common/AkTypes.h>
//#include <AK/SoundEngine/Common/AkMemoryMgr.h>      // Memory Manager
//#include <AK/SoundEngine/Common/AkModule.h>         // Default memory and stream managers
//
//using namespace AKPLATFORM;
//""")>]
//module Tests =
//
//    [<Import; MI (MIO.NoInlining)>]
//    let initWwiseMemoryManager () : int =
//        C """
//        //
//        // Create and initialize an instance of the default memory manager. Note
//        // that you can override the default memory manager with your own. Refer
//        // to the SDK documentation for more information.
//        //
//
//        AkMemSettings memSettings;
//        memSettings.uMaxNumPools = 20;
//
//        if ( AK::MemoryMgr::Init( &memSettings ) != AK_Success )
//        {
//            assert( ! "Could not create the memory manager." );
//            return 0;
//        }
//
//        return 1;
//        """
//
//    [<Export>]
//    let printInFSharp () : unit =
//        async {
//            do! Async.Sleep (1000)
//            printfn "baloni"
//        } |> Async.Start
//        printfn "HELLO THARRRRR"
//
//    [<Import; MI (MIO.NoInlining)>]
//    let getInt () : int =
//        C """return 1;"""
//
//    [<Import; MI (MIO.NoInlining)>]
//    let printSomething () : unit =
//        C """
//        Tests_printInFSharp ();
//        """

[<Register("AppDelegate")>]
type AppDelegate() = 
    inherit UIApplicationDelegate()
    member val Window = null with get, set
    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching(app, options) = 
        //let didWisePass = Tests.initWwiseMemoryManager ()
       // printfn "Did Wise Pass: %A" <| Tests.getInt ()
      //  Tests.printSomething ()
        //
        this.Window <- new UIWindow(UIScreen.MainScreen.Bounds)
        // If you have defined a root view controller, set it here:
        // this.Window.RootViewController <- new MyViewController ()
        this.Window.MakeKeyAndVisible()
        true
