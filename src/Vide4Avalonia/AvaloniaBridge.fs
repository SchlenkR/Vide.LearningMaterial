// ---------------------------------------------------------------
// This is the specialization for Avalonia
// ---------------------------------------------------------------
namespace Vide

open System
open Vide
open Avalonia.Controls

// The current basis for vide-implementations is the
// NodeModel (with NodeDocument and NodeContext), which
// actually works like a true node model (always many children).
// The node model doesn't distinguish between different container
// forms (e.g. single / multi content), so there are some casts
// needed here and runtime exceptions can occur. Anyway, content models can be modeled correctly at the API
// surface, using the Render-builders.

type AvaloniaDocument() =
    let makeEx parent child actionName =
        let childTypeName = match child with Some child -> child.GetType().Name | None -> "-"
        Exception $"Cannot perform '{actionName}' to node of type {parent.GetType().Name} (child type = {childTypeName})."
    interface INodeDocument<Control> with
        member _.EnsureChildAppendedAtIdx(parent, child, idx) =
            match box parent with
            | :? Panel as panel ->
                let children = panel.Children
                let insertChildAtRequestedIdx () = children.Insert(idx, child)
                match children.IndexOf(child) with
                | -1 -> insertChildAtRequestedIdx ()
                | currIdx when currIdx = idx -> ()
                | _ ->
                    children.Remove(child) |> ignore
                    insertChildAtRequestedIdx ()
            | :? ContentControl as cc ->
                // this could lead to strange behaviour when the API is not implemented correctly.
                if cc.Content <> child then
                    cc.Content <- child
            | _ -> raise <| makeEx parent (Some child) "EnsureChildAppendedAtIdx"
        member _.RemoveChild(parent, child) =
            match box parent with
            | :? Panel as panel -> panel.Children.Remove(child) |> ignore
            | :? ContentControl as cc -> cc.Content <- null
            | _ -> raise <| makeEx parent (Some child) "RemoveChild"
        member _.GetChildren(parent) =
            match box parent with
            | :? Panel as panel -> panel.Children |> Seq.toList
            | :? ContentControl as cc ->
                // TODO: Das ist alles sehr suboptimal hier (i.A.)
                match cc.Content with
                | :? Control as c -> [c]
                | _ -> []
            | _ -> []
        member _.ClearChildren(parent) =
            match box parent with
            | :? Panel as panel -> panel.Children.Clear()
            | :? ContentControl as cc -> cc.Content <- null
            | _ -> raise <| makeEx parent None "ClearChildren"
        member _.CreateTextNode(text) =
            let tn = new TextBlock(Text = text)
            let textNode =
                {
                    node = tn :> Control
                    getText = fun () -> tn.Text
                    setText = fun value -> tn.Text <- value
                }
            textNode
    interface IWpfishDocument<Control> with
        member _.CreateNodeOfType<'e when 'e : (new: unit -> 'e)>() =
            let e = new 'e()
            e, (box e) :?> Control

type AvaloniaContext(parent: Control) =
    inherit WpfishContext<Control>(parent, AvaloniaDocument())

module AvaloniaContext =
    let create<'e when 'e :> Control> (host: IHost) (thisNode: 'e) = AvaloniaContext(thisNode)


// --------------------------------------------------
// Specialized builder definitions
// --------------------------------------------------

type ComponentRetCnBuilder() =
    inherit ComponentRetCnBaseBuilder<Control,AvaloniaContext>()

type RenderValC0Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement, createResultVal)
type RenderPotC0Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, createResultVal) =
    inherit RenderValC0BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement, createResultVal)
type RenderRetC0Builder<'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement) =
    inherit RenderRetC0BaseBuilder<'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement)

type RenderValC1Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, createResultVal) =
    inherit RenderValC1BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement, createResultVal)
type RenderPotC1Builder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, createResultVal) =
    inherit RenderPotC1BaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement, createResultVal)
type RenderRetC1Builder<'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement) =
    inherit RenderRetC1BaseBuilder<'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement)

type RenderValCnBuilder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, createResultVal) =
    inherit RenderValCnBaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement, createResultVal)
type RenderPotCnBuilder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement, createResultVal) =
    inherit RenderPotCnBaseBuilder<'v,'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement, createResultVal)
type RenderRetCnBuilder<'e when 'e :> Control and 'e : (new: unit -> 'e)>(createThisElement) =
    inherit RenderRetCnBaseBuilder<'e,Control,AvaloniaContext>(AvaloniaContext.create, createThisElement)

module InitHelper =
    let addOnInit (this: #NodeBuilder<_,_>) onInit =
        this.InitModifiers.Add(fun nmodctx -> onInit(nmodctx.node))

/// A builder that works on controls that have no content and don't return a value; e.g. Label.
type ContentLeafRetBuilder<'e when 'e :> Control and 'e : (new: unit -> 'e)>(?onInit) as this =
    inherit RenderRetC0Builder<'e>(BuilderHelper.createNode)
    do match onInit with Some onInit -> InitHelper.addOnInit this onInit | None -> ()

/// A builder that works on controls that have no content and can return a value; e.g. TextBox.
type ContentLeafPotBuilder<'v,'e when 'e :> Control and 'e : (new: unit -> 'e)>(createResultVal, ?onInit) as this =
    inherit RenderPotC0Builder<'v,'e>(BuilderHelper.createNode, createResultVal)
    do match onInit with Some onInit -> InitHelper.addOnInit this onInit | None -> ()

/// A builder that works on a single content that don't return a value; e.g. Button.
type ContentControlRetBuilder<'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>(?onInit) as this =
    inherit RenderRetC1Builder<'e>(BuilderHelper.createNode)
    do match onInit with Some onInit -> InitHelper.addOnInit this onInit | None -> ()

/// A builder that works on a single content that can return a value; e.g. CheckBox.
type ContentControlPotBuilder<'v,'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>(createResultVal, ?onInit) as this =
    inherit RenderPotC1Builder<'v,'e>(BuilderHelper.createNode, createResultVal)
    do match onInit with Some onInit -> InitHelper.addOnInit this onInit | None -> ()

/// A builder that works on many children; e.g. StackPanel
type PanelRetBuilder<'e when 'e :> Panel and 'e :> Panel and 'e : (new: unit -> 'e)>(?onInit) as this =
    inherit RenderRetCnBuilder<'e>(BuilderHelper.createNode)
    do match onInit with Some onInit -> InitHelper.addOnInit this onInit | None -> ()


/// Avalonia App
type VideApp =
    static member ForHost(host) = 
        VideAppFactory(
            (fun () -> AvaloniaContext(host)),
            (fun ctx -> ctx.ctx.RemoveObsoleteChildren())
        )

[<AutoOpen>]
module VideBuilderInstance =
    let vide = ComponentRetCnBuilder()

type L<'e when 'e :> Control and 'e : (new: unit -> 'e)>(?onInit) = 
    inherit ContentLeafRetBuilder<'e>(?onInit = onInit)

type C<'e when 'e :> ContentControl and 'e : (new: unit -> 'e)>(?onInit) =
    inherit ContentControlRetBuilder<'e>(?onInit = onInit)

type P<'e when 'e :> Panel and 'e : (new: unit -> 'e)>(?onInit) = 
    inherit PanelRetBuilder<'e>(?onInit = onInit)

module Test =
    let view = vide {
        "Test"
        L<TextBlock>(fun x -> x.MaxLines <- 100)
    }
