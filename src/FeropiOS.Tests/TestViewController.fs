namespace FeropiOS.Tests

open UIKit
open CoreGraphics
open Foundation
open System

open Ferop

[<Ferop>]
[<ClangiOS("")>]
[<Source("""
int testValue = 0;
""")>]
module Tests =

    [<Export>]
    let printTestValue (value: int) : unit =
        printfn "Test Value is: %A" value

    [<Import; MI (MIO.NoInlining)>]
    let getTestValue () : int =
        C """
        return testValue;
        """

    [<Import; MI (MIO.NoInlining)>]
    let incrementTestValue () : unit =
        C """
        testValue++;
        Tests_printTestValue (testValue);
        """

type TestViewController() = 
    inherit UIViewController()

    override this.ViewDidLoad () =
        base.ViewDidLoad ()

        this.View.BackgroundColor <- UIColor.White

        let button = new UIButton (UIButtonType.System)
        button.TranslatesAutoresizingMaskIntoConstraints <- false

        button.SetTitle (Tests.getTestValue().ToString(), UIControlState.Normal)

        // This is bad; lasts forever. :(
        button.TouchUpInside.Add(fun _ -> 
            Tests.incrementTestValue()
            button.SetTitle (Tests.getTestValue().ToString(), UIControlState.Normal)
        )

        this.View.AddSubview(button)

        this.View.AddConstraint <|
            NSLayoutConstraint.Create(
                button, NSLayoutAttribute.Width, NSLayoutRelation.Equal, 
                null, NSLayoutAttribute.NoAttribute, nfloat 1., nfloat 256.
            )

        this.View.AddConstraint <|
            NSLayoutConstraint.Create(
                button, NSLayoutAttribute.Height, NSLayoutRelation.Equal, 
                null, NSLayoutAttribute.NoAttribute, nfloat 1., nfloat 64.
            )
         
        this.View.AddConstraint <|
            NSLayoutConstraint.Create(
                button, NSLayoutAttribute.CenterX, NSLayoutRelation.Equal, 
                this.View, NSLayoutAttribute.CenterX, nfloat 1., nfloat 0.
            )

        this.View.AddConstraint <|
            NSLayoutConstraint.Create(
                button, NSLayoutAttribute.CenterY, NSLayoutRelation.Equal, 
                this.View, NSLayoutAttribute.CenterY, nfloat 1., (this.NavigationController.NavigationBar.Frame.Height)
            )

    override this.ShouldAutorotateToInterfaceOrientation (toInterfaceOrientation) =
        if UIDevice.CurrentDevice.UserInterfaceIdiom = UIUserInterfaceIdiom.Phone then
           toInterfaceOrientation <> UIInterfaceOrientation.PortraitUpsideDown
        else
           true
