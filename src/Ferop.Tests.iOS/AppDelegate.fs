namespace Ferop.Tests.iOS

open System
open MonoTouch.UIKit
open MonoTouch.Foundation

open Ferop

#if DEBUG
type Native = FeropProvider<"Ferop.Tests.iOS.Native", "bin/iPhoneSimulator/Debug", Code.Platform.AppleiOS>
#else
type Native = FeropProvider<"Ferop.Tests.iOS.Native", "bin/iPhoneSimulator/Release", Code.Platform.AppleiOS>
#endif

[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit UIApplicationDelegate ()

    let window = new UIWindow (UIScreen.MainScreen.Bounds)

    // This method is invoked when the application is ready to run.
    override this.FinishedLaunching (app, options) =
        window.RootViewController <- new Ferop_Tests_iOSViewController ()
        window.MakeKeyAndVisible ()

        Console.WriteLine (Native.Tests.testByte (123uy))

        true

module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main (args, null, "AppDelegate")
        0

