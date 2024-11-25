namespace Vide

#if USE_SINGLECASEDU_WITHOUT_INLINEIFLAMBDA

type Vide<'v,'s,'c> = Vide of ('s option -> 'c -> 'v * 's option)

[<AutoOpen>]
module VideFunction =
    let inline mkVide v = Vide v
    let inline runVide (Vide v) = v

module IILShadowing =
    type InlineIfLambdaAttribute() = inherit System.Attribute()

#else

type Vide<'v,'s,'c> = ('s option -> 'c -> 'v * 's option)

[<AutoOpen>]
module VideFunction =
    let inline mkVide<'v,'s,'c> ([<InlineIfLambda>] v: Vide<'v,'s,'c>) = v
    let inline runVide<'v,'s,'c> ([<InlineIfLambda>] v: Vide<'v,'s,'c>) = v

module IILShadowing =
    type InlineIfLambdaAttribute = FSharp.Core.InlineIfLambdaAttribute

#endif

open IILShadowing

module Vide =

    // Preserves the first value given and discards subsequent values.
    let preserveValue<'v,'c> (x: 'v) =
        mkVide <| fun s (ctx: 'c) ->
            let s = s |> Option.defaultValue x
            s, Some s
    
    let preserveWith<'v,'c> (x: unit -> 'v) =
        mkVide <| fun s (ctx: 'c) ->
            let s = s |> Option.defaultWith x
            s, Some s
    
    // TODO: Think about which function is "global" and module-bound
    let inline map ([<InlineIfLambda>] proj: 'v1 -> 'v2) (v: Vide<'v1,'s,'c>) : Vide<'v2,'s,'c> =
        mkVide <| fun s ctx ->
            let v,s = (runVide v) s ctx
            proj v, s

    // ----- zero:
    // why are there 2 'zero' functions? -> see comment in "VideBuilder.Zero"
    // ------------------

    [<GeneralizableValue>]
    let zeroFixedState<'c> : Vide<unit,unit,'c> =
        mkVide <| fun s ctx -> (),None

    [<GeneralizableValue>]
    let zeroAdaptiveState<'s,'c> : Vide<unit,'s,'c> =
        mkVide <| fun s ctx -> (),s

    // ------------------

    [<GeneralizableValue>]
    let ctx<'c> : Vide<'c,unit,'c> =
        mkVide <| fun s ctx -> ctx,None

    let inline bind<'v1,'s1,'v2,'s2,'c>
        ([<InlineIfLambda>] f: 'v1 -> Vide<'v2,'s2,'c>)
        (m: Vide<'v1,'s1,'c>)
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        mkVide <| fun s ctx ->
            let ms,fs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let mv,ms = (runVide m) ms ctx
            let f = runVide (f mv)
            let fv,fs = f fs ctx
            fv, Some (ms,fs)

    let inline ofSeqWithDefault ([<InlineIfLambda>] defaultFunc) (sequence: seq<_>) =
        mkVide <| fun s ctx ->
            let enumerator =
                let newEnum () =
                    let x = sequence.GetEnumerator()
                    if not (x.MoveNext()) then defaultFunc () else x
                match s with
                | None -> newEnum ()
                | Some (x: System.Collections.Generic.IEnumerator<_>) ->
                    if x.MoveNext() then x else newEnum ()
            enumerator.Current, Some enumerator
    
    let ofSeqWithDefaultValue defaultValue (sequence: seq<_>) =
        ofSeqWithDefault (fun () -> defaultValue) sequence
    
    let ofSeq (sequence: seq<_>) =
        ofSeqWithDefault (fun () -> failwith "Empty sequences are not supported.") sequence

    // TODO: ofList / ofArray

module BuilderBricks =
    let inline bind<'v1,'s1,'v2,'s2,'c>
        (
            m: Vide<'v1,'s1,'c>,
            [<InlineIfLambda>] f: 'v1 -> Vide<'v2,'s2,'c>
        ) 
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        Vide.bind f m

    let return'<'v,'c>
        (x: 'v)
        : Vide<'v,unit,'c> 
        =
        mkVide <| fun s ctx -> x,None

    let yield'<'v,'s,'c>
        (v: Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        v

    // -----------------------
    // Zero:
    // -----------------------
    // This zero (with "unit" as state) is required for multiple returns.
    // Another zero (with 's as state) is required for "if"s without an "else".
    // We cannot have both, which means: We cannot have "if"s without "else".
    // This is ok (and not unfortunate), because the developer has to make a
    // decision about what should happen: "elseForget" or "elsePreserve".
    // -----------------------
    
    let zeroFixedState<'c>
        ()
        : Vide<unit,unit,'c>
        = Vide.zeroFixedState<'c>

    let zeroAdaptiveState<'s,'c>
        ()
        : Vide<unit,'s,'c>
        = Vide.zeroAdaptiveState<'s,'c>

    let inline delay<'v,'s,'c>
        ([<InlineIfLambda>] f: unit -> Vide<'v,'s,'c>)
        : Vide<'v,'s,'c>
        =
        f ()

    // TODO
    // those 2 are important for being able to combine
    // HTML elements, returns and asyncs in arbitrary order
    //member _.Combine
    //    (
    //        a: Vide<unit,'s1,'c>,
    //        b: Vide<'v,'s2,'c>
    //    )
    //    : Vide<'v,'s1 option * 's2 option,'c>
    //    =
    //    combine a b snd
    //member _.Combine
    //    (
    //        a: Vide<'v,'s1,'c>,
    //        b: Vide<unit,'s2,'c>
    //    )
    //    : Vide<'v,'s1 option * 's2 option,'c>
    //    =
    //    combine a b fst
    let combine<'v1,'s1,'v2,'s2,'c>
        (
           a: Vide<'v1,'s1,'c>,
           b: Vide<'v2,'s2,'c>
        )
        : Vide<'v2,'s1 option * 's2 option,'c>
        =
        mkVide <| fun s ctx ->
            let sa,sb =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let va,sa = (runVide a) sa ctx
            let vb,sb = (runVide b) sb ctx
            vb, Some (sa,sb)

    // 'for' is really a NodeModel, speific thing, so I moved it there.
    // let inline for'<'a,'v,'s,'c when 'a: comparison>
    //     (
    //         inpElems: seq<'a>,
    //         [<InlineIfLambda>] body: 'a -> Vide<'v,'s,'c>
    //     ) 
    //     : Vide<'v list, ResizeArray<'s option>, 'c>
    //     =
    //     mkVide <| fun s ctx ->

[<AutoOpen>]
module Keywords =
    
    [<GeneralizableValue>]
    let elsePreserve<'s,'c> : Vide<unit,'s,'c> =
        mkVide <| fun s ctx -> (),s

    [<GeneralizableValue>]
    let elseForget<'s,'c> : Vide<unit,'s,'c> = 
        mkVide <| fun s ctx -> (),None


type HostContext<'ctx> = { host: IHost; ctx: 'ctx }

and IHost =
    abstract member RequestEvaluation: unit -> unit
    abstract member SuspendEvaluation: unit -> unit
    abstract member ResumeEvaluation: unit -> unit
    abstract member IsEvaluating: bool
    abstract member HasPendingEvaluationRequests: bool
    abstract member EvaluationCount: uint64

type MutableValue<'a when 'a: equality>(initial: 'a, evalManager: IHost) =
    let mutable state = initial
    member _.Set(value) =
        // Not just a perf opt: prevent stack overflows (see demo case asyncHelloWorld)!
        if value <> state then
            do state <- value
            do evalManager.RequestEvaluation()
    member this.Reset() = this.Set(initial)
    member inline this.Update(op, value) =
        this.Value <- op this.Value value
    member this.Value
        with get() = state
        and set(value) = this.Set(value)

type StateCtor<'a> = StateCtor of (unit -> 'a)

type DelayedMutableBuilder() =
    member _.Yield(x) =
        mkVide <| fun s (ctx: HostContext<_>) ->
            let s = s |> Option.defaultWith (fun () -> MutableValue(x, ctx.host))
            s, Some s
    member _.Zero() = BuilderBricks.zeroAdaptiveState
    member _.Combine(a, b) = BuilderBricks.combine(a, b ())
    member _.Delay(f) = f
    member _.Run(f) = StateCtor f

module State =
    let ofMutable = DelayedMutableBuilder()

open System

type OnEvalCallbackArgs<'v,'s> =
    { evaluationCount: uint64
      value: 'v
      currentState: 's option
      duration: TimeSpan }

type VideApp<'v,'s,'c>
    (
        content: Vide<'v,'s,HostContext<'c>>,
        ctxCtor: unit -> 'c,
        ctxFin: HostContext<'c> -> unit
    ) =
    let mutable currValue = None
    let mutable currentState = None
    let mutable isEvaluating = false
    let mutable hasPendingEvaluationRequests = false
    let mutable evaluationCount = 0uL
    let mutable suspendEvaluation = false
    let mutable onEvaluated: (OnEvalCallbackArgs<'v,'s> -> unit) option = None

    // TODO: rename to IHost or IRoot
    interface IHost with
        member this.RequestEvaluation() =
            if suspendEvaluation then
                hasPendingEvaluationRequests <- true
            else
                // During an evaluation, requests for another evaluation can
                // occur, which have to be handled as _subsequent_ evaluations!
                let rec eval () =
                    let startTime = DateTime.Now
                    do
                        hasPendingEvaluationRequests <- false
                        isEvaluating <- true
                    let value,newState = 
                        let ctx = { host = this; ctx = ctxCtor () }
                        let res = (runVide content) currentState ctx
                        do ctxFin ctx
                        res
                    do
                        currValue <- Some value
                        currentState <- newState
                        isEvaluating <- false
                        evaluationCount <- evaluationCount + 1uL
                    do
                        let diag = 
                            { 
                                evaluationCount = evaluationCount
                                value = value
                                currentState = currentState
                                duration = DateTime.Now - startTime
                            }
                        onEvaluated |> Option.iter (fun callback -> callback diag)
                    if hasPendingEvaluationRequests then
                        eval ()
                do
                    match isEvaluating with
                    | true -> hasPendingEvaluationRequests <- true
                    | false -> eval ()
        member _.SuspendEvaluation() =
            do suspendEvaluation <- true
        member this.ResumeEvaluation() =
            do suspendEvaluation <- false
            if hasPendingEvaluationRequests then
                (this :> IHost).RequestEvaluation()
        member _.IsEvaluating = isEvaluating
        member _.HasPendingEvaluationRequests = hasPendingEvaluationRequests
        member _.EvaluationCount = evaluationCount

    member this.EvaluationManager = this :> IHost
    member _.CurrentState = currentState
    member this.ForceState(state) =
        do currentState <- Some state
        this.EvaluationManager.RequestEvaluation()
    member _.OnEvaluated(evaluationCallback) = onEvaluated <- Some evaluationCallback
    member _.OnEvaluated() = onEvaluated <- None

type VideAppFactory<'c>(ctxCtor, ctxFin) =
    let start (app: VideApp<_,_,'c>) =
        do app.EvaluationManager.RequestEvaluation()
        app
    member _.Create(content) : VideApp<_,_,'c> =
        VideApp(content, ctxCtor, ctxFin)
    member this.CreateWithUntypedState(content: Vide<_,_,_>) : VideApp<_,_,'c> =
        let content =
            mkVide <| fun (s: obj option) ctx ->
                let typedS = s |> Option.map (fun s -> s :?> 's)
                let v,s = (runVide content) typedS ctx
                let untypedS = s |> Option.map (fun s -> s :> obj)
                v,untypedS
        this.Create(content)
    member this.CreateAndStart(content) : VideApp<_,_,'c> =
        this.Create(content) |> start
    member this.CreateAndStartWithUntypedState(content) : VideApp<_,_,'c> =
        this.CreateWithUntypedState(content) |> start


open System.Runtime.CompilerServices

type NodeModelBaseBuilder() =
    member _.Bind(StateCtor m, f) =
        mkVide <| fun s ctx ->
            let m = m ()
            let bindRes = BuilderBricks.bind(m, f)
            runVide bindRes s ctx
    member _.Bind(m, f) = BuilderBricks.bind(m, f)
    member _.Zero() = BuilderBricks.zeroFixedState()

type TextNodeProxy<'n> =
    {
        node: 'n
        getText: unit -> string
        setText: string -> unit
    }

[<Struct>]
type KVP<'k,'v> = KVP of 'k * 'v

type INodeDocument<'n> =
    abstract member EnsureChildAppendedAtIdx : parent:'n * child:'n * idx:int -> unit
    abstract member RemoveChild : parent: 'n * child: 'n -> unit
    abstract member GetChildren : parent: 'n -> 'n list
    abstract member ClearChildren : parent: 'n -> unit
    // This seems to be so common and useful for all type of backends
    // that we will leave it here (for now)
    abstract member CreateTextNode : text: string -> TextNodeProxy<'n>

[<AbstractClass>] 
type NodeContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: INodeDocument<'n>
    ) =
    let mutable keptChildren = []
    member _.CreateTextNode(value: string) =
        document.CreateTextNode(value)
    member _.ShowChild(child) =
        // What is important here:
        // The ordering of elements can change; important for "for".
        do keptChildren <- child :: keptChildren
        do document.EnsureChildAppendedAtIdx(parent, child, keptChildren.Length - 1)
    member _.RemoveObsoleteChildren() =
        let childrenForRemoval = document.GetChildren(parent) |> List.except keptChildren
        for child in childrenForRemoval do
            document.RemoveChild(parent, child)
    member _.ClearContent() =
        do document.ClearChildren(parent)

type BuilderOperations = | Clear

type NodeBuilderState<'e,'s> = option<'e> * option<'s>

type NodeModifierContext<'e> = 
    { 
        node: 'e
        host: IHost
    }

type NodeModifier<'n> = NodeModifierContext<'n> -> unit

// TODO: Since having removed checkChildNode, we could remove 'n.
// Does that open ways for simplification?
[<AbstractClass>]
type NodeBuilder<'e,'c>
    (
        createContext: IHost -> 'e -> 'c,
        createThisElement: IHost -> 'c -> 'e
    ) =
    inherit NodeModelBaseBuilder()

    member val InitModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val PreEvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get
    member val PostEvalModifiers: ResizeArray<NodeModifier<'e>> = ResizeArray() with get

    member _.Delay(f) = BuilderBricks.delay<_,_,HostContext<'c>>(f)

    member _.CreateContext = createContext
    member _.CreateThisElement = createThisElement

type YieldingNodeBuilder<'e,'c>
    (
        createContext: IHost -> 'e -> 'c,
        createThisElement: IHost -> 'c -> 'e
    ) =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)


module NodeModelBuilderBricks =
    // TODO: This is really, really weired. I think it's necessary to distinguish
    // between 'nthis and 'nhild on a general level (see branch of 2023-02-18 and many others)
    (*
        Generic Argument names:
        ---

        'e : A concrete element type, e.g. `HTMLButtonElement`
        'n : The abstract node type that is the basis for
             tree composition (e.g. `Node`)
        ...

        Notes
        ---
        
        In order to get rid of the unsafe cast, we need a constraint in form of ('e :> 'n),
        which is not possible. The interesting thing to see here is the type of "this":
            NodeBuilder<'e,'n,'c> = NodeBuilder<'e,'n,#NodeContext<'n>>
            
        Also, it is in general required that the builders have a concrete and completely
        specialized 'c (context) in Vide. This ensures smooth composition and overload
        resolution of the builder methods in the CEs, and it makes CE builder methods
        easy to implement (e.g.: see "Combine").

        // TODO: Is this still valid? `checkChildNode` was removed, so maybe we can do this again?
        
        Even if NodeBuilder has now 'e _and also_ 'n, we still have to use an unsafe cast from
        'e to 'n, but having also 'n as arg in NodeBuilder, we can have `checkChildNode`
        taking a 'n instead of an 'e (which otherwise would be wrong).
    *)
    let inline run<'v1,'v2,'s,'e,'n,'c
            when 'n: equality
            and 'c :> NodeContext<'n>
        >
        (
            thisBuilder: NodeBuilder<'e,'c>,
            childVide: Vide<'v1, 's, HostContext<'c>>,
            createResultVal: 'e -> 'v1 -> 'v2
        )
        : Vide<'v2, NodeBuilderState<'e,'s>, HostContext<'c>>
        =
        mkVide <| fun s parentCtx ->
            let inline runModifiers modifiers node =
                for m in modifiers do
                    m { node = node; host = parentCtx.host }
            let s,cs =
                match s with
                | None -> None,None
                | Some (ms,fs) -> ms,fs
            let thisElement,cs =
                // Can it happen that s is Some and cs is None? I don't think so.
                // But: See comment in definition of: Vide.Core.Vide
                match s with
                | None ->
                    let newElement,s = thisBuilder.CreateThisElement parentCtx.host parentCtx.ctx, cs
                    do runModifiers thisBuilder.InitModifiers newElement
                    newElement,s
                | Some thisElement ->
                    do parentCtx.ctx.ShowChild(box thisElement :?> 'n)
                    thisElement,cs
            do runModifiers thisBuilder.PreEvalModifiers thisElement
            let thisCtx =
                let newCtx = thisBuilder.CreateContext parentCtx.host thisElement
                { parentCtx with ctx = newCtx }
            let cv,cs = (runVide childVide) cs thisCtx
            do thisCtx.ctx.RemoveObsoleteChildren()
            do runModifiers thisBuilder.PostEvalModifiers thisElement
            let result = createResultVal thisElement cv
            let state = Some (Some thisElement, cs)
            result,state

    let inline forWithKVP<'a,'key,'v,'s,'c when 'key: comparison>
        (
            elems: seq<KVP<'key, 'a>>,
            [<InlineIfLambda>] body: 'a -> Vide<'v,'s,'c>
        )
        : Vide<'v list, Map<'key, 's option>, 'c>
        =
        mkVide <| fun s ctx ->
            let mutable currMap = s |> Option.defaultValue Map.empty
            let resValues,resStates =
                [ for KVP(key,elem) in elems do
                    let matchingState =
                        let found,maybeValue = currMap.TryGetValue(key)
                        if found then maybeValue else None
                    let v,s =
                        let v = runVide (body elem)
                        v matchingState ctx
                    do currMap.Remove(key) |> ignore
                    v, (key,s)
                ]
                |> List.unzip
            let newState = resStates |> Map.ofList
            if newState.Count <> resStates.Length then
                failwith "Duplicate key in forWithKVP"
            resValues, Some newState

    let inline forWithKeyField<^a,'key,'v,'s,'c
            when 'key: comparison
            and ^a: (member key: 'key)
        >
        (
            input: seq<^a>,
            [<InlineIfLambda>] body
        )
        =
        let input = input |> Seq.map (fun x -> KVP (x.key,x))
        forWithKVP<^a,'key,'v,'s,'c> (input, body)

    let inline yieldVide(v: Vide<_,_,_>) =
        v
    
    let inline yieldBuilderOp<'n,'c when 'c :> NodeContext<'n>>(op: BuilderOperations) =
        mkVide <| fun s (ctx: HostContext<'c>) ->
            match op with | Clear -> do ctx.ctx.ClearContent()
            (),None

    let inline yieldText<'n,'c when 'c :> NodeContext<'n>>(value: string) =
        mkVide <| fun s (ctx: HostContext<'c>) ->
            let textNode =
                s |> Option.defaultWith (fun () ->
                    let textNode = ctx.ctx.CreateTextNode(value)
                    do ctx.ctx.ShowChild(textNode.node)
                    textNode
                )
            do
                if textNode.getText() <> value then
                    textNode.setText(value)
                do ctx.ctx.ShowChild(textNode.node)
            (), Some textNode


// ---------------------------------------------------------------------------------
// The four (+1 base) basic builders for "vide { .. }" and renderers
// Used for HTML elements like
//    div, p, etc.in their forms (with content, with returns, specific
//    result value like for "input"), and the vide component builder.
// ---------------------------------------------------------------------------------

// ---------------------------------------------------------------------------------
// Builder definitions
//   + Run
//   + Return
//     - every Content builder should bind every other builder)
//     - standard yields
//   + Combine,For,Delay
// -------
// Note on 
//     "Pot" (has potential return value) and 
//     "Val" (has return value):
//     -> Transformation by "emitValue" member of Pot builders imply that
//          - derived classes have no additional state at all and
//          - "emitValue" is not part of the fluent API and shall be called
//            as the last element in the chain (from a user's perspective).
// ---------------------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n>
    > () =
    inherit NodeModelBaseBuilder()
    member _.Return(x) = BuilderBricks.return'<_,HostContext<'c>>(x)
    member inline _.Delay([<IILShadowing.InlineIfLambda>] f) = BuilderBricks.delay<_,_,HostContext<'c>>(f)
    member inline _.Combine([<IILShadowing.InlineIfLambda>] a, [<IILShadowing.InlineIfLambda>] b) = BuilderBricks.combine<_,_,_,_,HostContext<'c>>(a, b)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKVP<_,_,_,_,HostContext<'c>>(seq, body)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKeyField<_,_,_,_,HostContext<'c>>(seq, body)

type RenderValC0BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> createResultVal n))

type RenderPotC0BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> v))
    member _.emitValue() = RenderValC0BaseBuilder(createContext, createThisElement, createResultVal)

type RenderRetC0BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement) 
    =
    inherit NodeBuilder<'e,'c>(createContext, createThisElement)
    member _.Return(x) = BuilderBricks.return'<_,HostContext<'c>>(x)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> v))

and RenderValC1BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit YieldingNodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> createResultVal n))

type RenderPotC1BaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit YieldingNodeBuilder<'e,'c>(createContext, createThisElement)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> v))
    member _.emitValue() = RenderValC1BaseBuilder(createContext, createThisElement, createResultVal)

type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement) 
    =
    inherit YieldingNodeBuilder<'e,'c>(createContext, createThisElement)
    member _.Return(x) = BuilderBricks.return'<_,HostContext<'c>>(x)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> v))

type RenderValCnBaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v)
    =
    inherit YieldingNodeBuilder<'e,'c>(createContext, createThisElement)
    member inline _.Combine([<IILShadowing.InlineIfLambda>] a, [<IILShadowing.InlineIfLambda>] b) = BuilderBricks.combine<_,_,_,_,HostContext<'c>>(a, b)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKVP<_,_,_,_,HostContext<'c>>(seq, body)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKeyField<_,_,_,_,HostContext<'c>>(seq, body)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> createResultVal n))

type RenderPotCnBaseBuilder<'v,'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement, createResultVal: 'e -> 'v) 
    =
    inherit YieldingNodeBuilder<'e,'c>(createContext, createThisElement)
    member inline _.Combine([<IILShadowing.InlineIfLambda>] a, [<IILShadowing.InlineIfLambda>] b) = BuilderBricks.combine<_,_,_,_,HostContext<'c>>(a, b)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKVP<_,_,_,_,HostContext<'c>>(seq, body)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKeyField<_,_,_,_,HostContext<'c>>(seq, body)
    member this.Run(v) = NodeModelBuilderBricks.run(this, v, (fun n v -> createResultVal n))
    member _.emitValue() = RenderValCnBaseBuilder(createContext, createThisElement, createResultVal)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    >
    (createContext, createThisElement) 
    =
    inherit YieldingNodeBuilder<'e,'c>(createContext, createThisElement)
    member inline _.Combine([<IILShadowing.InlineIfLambda>] a, [<IILShadowing.InlineIfLambda>] b) = BuilderBricks.combine<_,_,_,_,HostContext<'c>>(a, b)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKVP<_,_,_,_,HostContext<'c>>(seq, body)
    member inline _.For(seq, body) = NodeModelBuilderBricks.forWithKeyField<_,_,_,_,HostContext<'c>>(seq, body)
    member inline this.Run([<IILShadowing.InlineIfLambda>] v) = NodeModelBuilderBricks.run(this, v, (fun n v -> v))
    member _.Return(x) = BuilderBricks.return'(x)


// -------------------------------------------------------------------
// "Yielsd"s 
//     - every Content builder should bind every other builder)
//     - standard yields
// -------------------------------------------------------------------

type ComponentRetCnBaseBuilder<'n,'c
        when 'n : equality
        and 'c :> NodeContext<'n>
    > with
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderPotC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderValC1BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderPotC1BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC1BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(v) = NodeModelBuilderBricks.yieldVide(v)
    member _.Yield(op) = NodeModelBuilderBricks.yieldBuilderOp(op)
    member _.Yield(text) = NodeModelBuilderBricks.yieldText(text)

type YieldingNodeBuilder<'e,'c> with
    member _.Yield(b: ComponentRetCnBaseBuilder<_,_>) = b {()}
    member _.Yield(b: RenderValC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderPotC0BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC0BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderValC1BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderPotC1BaseBuilder<_,_,_,_>) = b {()}
    member _.Yield(b: RenderRetC1BaseBuilder<_,_,_>) = b {()}
    member _.Yield(b: RenderRetCnBaseBuilder<_,_,_>) = b {()}
    member _.Yield(v) = NodeModelBuilderBricks.yieldVide(v)
    member _.Yield(op) = NodeModelBuilderBricks.yieldBuilderOp(op)
    member _.Yield(text) = NodeModelBuilderBricks.yieldText(text)


// ----------------------------------------------------------------------------
// "Bind"s (every Content builder can bind every builder that returns values)
// ----------------------------------------------------------------------------

type RenderRetC1BaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderValC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC1BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type RenderRetCnBaseBuilder<'e,'n,'c
        when 'n: equality
        and 'c :> NodeContext<'n>
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderValC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC1BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

type ComponentRetCnBaseBuilder<'n,'c
        when 'c :> NodeContext<'n> 
        and 'n : equality
    > with
    member _.Bind(m: RenderValC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC0BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC0BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderValC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderPotC1BaseBuilder<_,_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetC1BaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: RenderRetCnBaseBuilder<_,_,_>, f) = BuilderBricks.bind(m {()}, f)
    member _.Bind(m: ComponentRetCnBaseBuilder<_,_>, f) = BuilderBricks.bind(m {()}, f)

[<Extension>]
type NodeBuilderExtensions =
    
    /// Called once on initialization.
    [<Extension>]
    static member onInit(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.InitModifiers.Add(m)
        this
    
    /// Called on every Vide evaluatiopn cycle.
    [<Extension>]
    static member onEval(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.PreEvalModifiers.Add(m)
        this
    
    /// Called after every Vide evaluatiopn cycle.
    [<Extension>]
    static member onAfterEval(this: #NodeBuilder<_,_>, m: NodeModifier<_>) =
        do this.PostEvalModifiers.Add(m)
        this

module Event =
    type NodeEventArgs<'evt,'e> =
        {
            node: 'e
            evt: 'evt
            host: IHost
            mutable requestEvaluation: bool
        }

    // TODO: InlineIfLambda
    let inline handle
        (node: 'e)
        (host: IHost)
        (callback: NodeEventArgs<'evt,'e> -> unit)
        =
        fun evt ->
            let args = { node = node; evt = evt; host = host; requestEvaluation = true }
            try
                do host.SuspendEvaluation()
                do callback args
                if args.requestEvaluation then
                    host.RequestEvaluation()
            finally
                do host.ResumeEvaluation()

[<RequireQualifiedAccess>]
module For =
    let keyed elems = elems |> Seq.map KVP
    let selfKeyed elems = elems |> Seq.map (fun x -> KVP (x,x))

type IWpfishDocument<'n when 'n: equality> =
    inherit INodeDocument<'n>
    abstract member CreateNodeOfType<'e when 'e : (new: unit -> 'e)> : unit -> 'e * 'n

[<AbstractClass>] 
type WpfishContext<'n when 'n: equality>
    (
        parent: 'n, 
        document: IWpfishDocument<'n>
    ) =
    inherit NodeContext<'n>(parent, document)
    member _.WpfishDocument = document

module BuilderHelper =
    // TODO: This should not append - should be done in "apply"
    let createNode<'e,'n when 'n: equality and 'e : (new: unit -> 'e)> =
        fun (host: IHost) (ctx: WpfishContext<'n>) ->
            let e,n = ctx.WpfishDocument.CreateNodeOfType<'e>()
            do ctx.ShowChild(n)
            e


// ---------------------------------------------------------------
// This is the specialization for Avalonia
// ---------------------------------------------------------------

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


// --------------------------------------------------
// App
// --------------------------------------------------
    
type VideApp =
    static member ForHost(host) = 
        VideAppFactory(
            (fun () -> AvaloniaContext(host)),
            (fun ctx -> ctx.ctx.RemoveObsoleteChildren())
        )

[<AutoOpen>]
module VideBuilderInstance =
    let vide = ComponentRetCnBuilder()
