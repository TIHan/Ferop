namespace FeropiOS.Tests

open System
open UIKit
open Foundation

[<Register("AppDelegate")>]
type AppDelegate() = 
    inherit UIApplicationDelegate()

    override val Window = null with get, set

    override this.FinishedLaunching(app, options) = 
        let testController = new TestViewController ()
        let navigationController = new UINavigationController (testController)

        this.Window <- new UIWindow(UIScreen.MainScreen.Bounds)
        this.Window.RootViewController <- navigationController
        this.Window.MakeKeyAndVisible()
        true
