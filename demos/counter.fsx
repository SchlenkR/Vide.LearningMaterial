
#r "nuget: Avalonia, 11.2.1"
#r "nuget: Avalonia.Desktop, 11.2.1"
#r "nuget: Avalonia.Themes.Fluent, 11.2.1"

#I "../src/Vide4Avalonia/bin/Debug/netstandard2.0/publish"
#r "Vide4Avalonia.dll"

Vide4Avalonia.FSI.guardInit ()

// ^ -------------------------------------------------------------
// |_ This is the boilerplate to make the sample work in fsi.
//    Evaluate this _once and separate_ from the rest of the sample.
// ---------------------------------------------------------------



// ---------------------------------------------------------------
// Here starts the actual sample ...
// ---------------------------------------------------------------

open Vide4Avalonia
open Avalonia.Controls

let counterComponent = vide {
    let! count = ofMutable { 0 }

    a<StackPanel>(fun x -> 
        x.Orientation <- Orientation.Horizontal
        x.Margin <- Thickness 5
        ) {
        a<Button>(fun x -> 
            x.Content <- "DEC")
            .on(Button.ClickEvent, fun x -> count.Value <- count.Value - 1)
        a<TextBlock>(fun x -> 
            x.FontWeight <- FontWeight.Bold
            x.Text <- count.Value.ToString()
            x.Margin <- Thickness 5
            x.VerticalAlignment <- VA.Center
            )
        a<Button>(fun x ->
            x.Content <- "INC")
            .on(Button.ClickEvent, fun x -> count.Value <- count.Value + 1)
    }
}

let app = vide {
    a<StackPanel>(fun x -> 
        x.VerticalAlignment <- VA.Center
        x.HorizontalAlignment <- HA.Center
        ) {
        // the 1st counter
        counterComponent

        // the snd counter
        counterComponent
    }
}

FSI.createWindow 300. 500. |> FSI.showView app

