
#r "nuget: Avalonia, 11.0.0"
#r "nuget: Avalonia.Desktop, 11.0.0"
#r "nuget: Avalonia.Themes.Fluent, 11.0.0"

#I "Vide4Avalonia/bin/Debug/netstandard2.0/publish"
#r "Vide4Avalonia.dll"

Vide4Avalonia.FSI.guardInit ()

// ^ -------------------------------------------------------------
// |_ This is the boilerplate to make the sample work in fsi.
//    Evaluate this _once and separate_ from the rest of the sample.
// ---------------------------------------------------------------



// ---------------------------------------------------------------
// Here starts the actual sample ...
// ---------------------------------------------------------------

open System
open Avalonia
open Vide4Avalonia
open Vide4Avalonia.FSI.Dynamic
open Avalonia.Controls

type TodoList = { items: TodoItem list }
and TodoItem = { text: string; mutable isDone: bool; key: int }


let demoApp1 = vide {
    a<StackPanel>() {
        a<TextBlock>(fun x -> 
            x.MaxLines <- 100
            x.Text <- "Hello 1")
        a<TextBlock>(fun x -> 
            x.MaxLines <- 100
            x.Text <- "Hello 2")
        a<TextBlock>(fun x -> 
            x.MaxLines <- 100
            x.Text <- "Hello 3")
    }
}


let todoApp = vide {
    let! todoList = ofMutable {
        { 
            items = 
                [
                    {| text = "Write Vide docu"; isDone = false |}
                    {| text = "Cook new ramen broth"; isDone = false |}
                    {| text = "Stuff that's already done"; isDone = true |}
                    {| text = "Auto-gen Vide Avalonia API"; isDone = true |}
                    {| text = "Fix a Trulla C# codegen bug"; isDone = false |}
                    {| text = "Make a Trulla version for node"; isDone = false |}
                    {| text = "Write a LSP for Trulla templates"; isDone = false |}
                ]
                |> List.mapi (fun i x -> { text = x.text; isDone = x.isDone ;key = i })
        }
    }
    
    let setItems items = todoList.Value <- { todoList.Value with items = items }
        
    a<DockPanel>(fun x -> x.Margin <- Thickness(5)) {
        a<TextBlock>(fun x ->
            x.FontWeight <- FontWeight.Bold
            x.HorizontalAlignment <- HA.Center
            x.Text <- "My TODO List"
            x.AP DockPanel.DockProperty Dock.Top)
        a<DockPanel>(fun x ->
            x.Margin <- Thickness 4
            x.AP DockPanel.DockProperty Dock.Bottom) {
            
            let! itemName = ofMutable {""}

            a<Button>(fun x ->
                x.AP DockPanel.DockProperty Dock.Right
                x.Margin <- Thickness 0
                x.IsEnabled <- String.IsNullOrWhiteSpace(itemName.Value) |> not
                x.IsDefault <- true)
                .on(Button.ClickEvent, fun x ->
                    let nextId = 
                        match  todoList.Value.items |> List.map (fun x -> x.key) |> List.sortDescending with
                        | [] -> 0
                        | x::_ -> x + 1
                    let newItem = { text = itemName.Value; isDone = false; key = nextId }
                    do setItems (newItem :: todoList.Value.items)
                    do itemName.Reset())
                {
                    a<TextBlock>(fun x -> x.Text <- "Add Item")
                }

            a<TextBox>(fun x ->
                itemName.Value <- x.Text)
        }

        a<StackPanel>(fun x ->
            x.Margin <- Thickness 4
            x.Orientation <- Orientation.Vertical) 
            {
                for item in todoList.Value.items do
                    a<DockPanel>() {
                        a<Button>(fun x ->
                            x.IsEnabled <- item.isDone
                            x.AP DockPanel.DockProperty Dock.Right)
                            .on(Button.ClickEvent, fun x -> setItems (todoList.Value.items |> List.except [item]))
                            { 
                                a<TextBlock>(fun x -> x.Text <- "Remove")
                            }
                        a<CheckBox>(fun x ->
                            x.IsChecked <- item.isDone)
                            .on(CheckBox.IsCheckedChangedEvent, fun x ->
                                item.isDone <- Option.ofNullable x.IsChecked |> Option.defaultValue true
                            )
                        a<TextBlock>(fun x ->
                            x.VerticalAlignment <- VA.Center
                            x.TextTrimming <- TextTrimming.CharacterEllipsis
                            x.Text <- item.text)
                    }
        }
    }
}


let window = FSI.createWindow 300. 500.
let videApp = FSI.showView todoApp window

videApp.OnEvaluated(fun diag -> printfn $"EVAL (count = {diag.evaluationCount})")

