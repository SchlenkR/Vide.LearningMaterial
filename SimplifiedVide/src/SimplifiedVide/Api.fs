namespace Vide

open System.Runtime.CompilerServices
open Browser.Types
open Vide


[<AutoOpen>]
module HtmlEnumAttributeTypes =
    
    module a =
        
        [<RequireQualifiedAccess>]
        type ``media`` = 
        | ``and`` 
        | ``not`` 
        | ``,`` 
        
        [<RequireQualifiedAccess>]
        type ``referrerpolicy`` = 
        | ``no-referrer`` 
        | ``no-referrer-when-downgrade`` 
        | ``origin`` 
        | ``origin-when-cross-origin`` 
        | ``same-origin`` 
        | ``strict-origin-when-cross-origin`` 
        | ``unsafe-url`` 
        
        [<RequireQualifiedAccess>]
        type ``rel`` = 
        | ``alternate`` 
        | ``author`` 
        | ``bookmark`` 
        | ``external`` 
        | ``help`` 
        | ``license`` 
        | ``next`` 
        | ``nofollow`` 
        | ``noopener`` 
        | ``noreferrer`` 
        | ``prev`` 
        | ``search`` 
        | ``tag`` 
        
        [<RequireQualifiedAccess>]
        type ``target`` = 
        | ``_blank`` 
        | ``_self`` 
        | ``_parent`` 
        | ``_top`` 
        | ``framename`` 
        
    
    module area =
        
        [<RequireQualifiedAccess>]
        type ``coords`` = 
        | ``x1,y1,x2,y2`` 
        | ``x,y,radius`` 
        | ``x1,y1,x2,y2,__,xn,yn`` 
        
        [<RequireQualifiedAccess>]
        type ``media`` = 
        | ``and`` 
        | ``not`` 
        | ``,`` 
        
        [<RequireQualifiedAccess>]
        type ``referrerpolicy`` = 
        | ``no-referrer`` 
        | ``no-referrer-when-downgrade`` 
        | ``origin`` 
        | ``origin-when-cross-origin`` 
        | ``same-origin`` 
        | ``strict-origin-when-cross-origin`` 
        | ``unsafe-url`` 
        
        [<RequireQualifiedAccess>]
        type ``rel`` = 
        | ``alternate`` 
        | ``author`` 
        | ``bookmark`` 
        | ``help`` 
        | ``license`` 
        | ``next`` 
        | ``nofollow`` 
        | ``noreferrer`` 
        | ``prefetch`` 
        | ``prev`` 
        | ``search`` 
        | ``tag`` 
        
        [<RequireQualifiedAccess>]
        type ``shape`` = 
        | ``default`` 
        | ``rect`` 
        | ``circle`` 
        | ``poly`` 
        
        [<RequireQualifiedAccess>]
        type ``target`` = 
        | ``_blank`` 
        | ``_self`` 
        | ``_parent`` 
        | ``_top`` 
        | ``framename`` 
        
    
    module audio =
        
        [<RequireQualifiedAccess>]
        type ``preload`` = 
        | ``auto`` 
        | ``metadata`` 
        | ``none`` 
        
    
    module base' =
        
        [<RequireQualifiedAccess>]
        type ``target`` = 
        | ``_blank`` 
        | ``_self`` 
        | ``_parent`` 
        | ``_top`` 
        
    
    module bdo =
        
        [<RequireQualifiedAccess>]
        type ``dir`` = 
        | ``ltr`` 
        | ``rtl`` 
        
    
    module button =
        
        [<RequireQualifiedAccess>]
        type ``formenctype`` = 
        | ``application_x-www-form-urlencoded`` 
        | ``multipart_form-data`` 
        | ``text_plain`` 
        
        [<RequireQualifiedAccess>]
        type ``formmethod`` = 
        | ``get`` 
        | ``post`` 
        
        [<RequireQualifiedAccess>]
        type ``formtarget`` = 
        | ``_blank`` 
        | ``_self`` 
        | ``_parent`` 
        | ``_top`` 
        | ``framename`` 
        
        [<RequireQualifiedAccess>]
        type ``type`` = 
        | ``button`` 
        | ``submit`` 
        | ``reset`` 
        
    
    module form =
        
        [<RequireQualifiedAccess>]
        type ``autocomplete`` = 
        | ``on`` 
        | ``off`` 
        
        [<RequireQualifiedAccess>]
        type ``enctype`` = 
        | ``application_x-www-form-urlencoded`` 
        | ``multipart_form-data`` 
        | ``text_plain`` 
        
        [<RequireQualifiedAccess>]
        type ``method`` = 
        | ``get`` 
        | ``post`` 
        
        [<RequireQualifiedAccess>]
        type ``rel`` = 
        | ``external`` 
        | ``help`` 
        | ``license`` 
        | ``next`` 
        | ``nofollow`` 
        | ``noopener`` 
        | ``noreferrer`` 
        | ``opener`` 
        | ``prev`` 
        | ``search`` 
        
        [<RequireQualifiedAccess>]
        type ``target`` = 
        | ``_blank`` 
        | ``_self`` 
        | ``_parent`` 
        | ``_top`` 
        | ``framename`` 
        
    
    module iframe =
        
        [<RequireQualifiedAccess>]
        type ``referrerpolicy`` = 
        | ``no-referrer`` 
        | ``no-referrer-when-downgrade`` 
        | ``origin`` 
        | ``origin-when-cross-origin`` 
        | ``same-origin`` 
        | ``strict-origin`` 
        | ``strict-origin-when-cross-origin`` 
        | ``unsafe-url`` 
        
        [<RequireQualifiedAccess>]
        type ``sandbox`` = 
        | ``(no value)`` 
        | ``allow-forms`` 
        | ``allow-modals`` 
        | ``allow-orientation-lock`` 
        | ``allow-pointer-lock`` 
        | ``allow-popups`` 
        | ``allow-popups-to-escape-sandbox`` 
        | ``allow-presentation`` 
        | ``allow-same-origin`` 
        | ``allow-scripts`` 
        | ``allow-top-navigation`` 
        | ``allow-top-navigation-by-user-activation`` 
        
    
    module img =
        
        [<RequireQualifiedAccess>]
        type ``loading`` = 
        | ``eager`` 
        | ``lazy`` 
        
        [<RequireQualifiedAccess>]
        type ``referrerpolicy`` = 
        | ``no-referrer`` 
        | ``no-referrer-when-downgrade`` 
        | ``origin`` 
        | ``origin-when-cross-origin`` 
        | ``unsafe-url`` 
        
    
    module input =
        
        [<RequireQualifiedAccess>]
        type ``accept`` = 
        | ``file_extension`` 
        | ``audio__`` 
        | ``video__`` 
        | ``image__`` 
        | ``media_type`` 
        
        [<RequireQualifiedAccess>]
        type ``autocomplete`` = 
        | ``on`` 
        | ``off`` 
        
        [<RequireQualifiedAccess>]
        type ``formenctype`` = 
        | ``application_x-www-form-urlencoded`` 
        | ``multipart_form-data`` 
        | ``text_plain`` 
        
        [<RequireQualifiedAccess>]
        type ``formmethod`` = 
        | ``get`` 
        | ``post`` 
        
        [<RequireQualifiedAccess>]
        type ``formtarget`` = 
        | ``_blank`` 
        | ``_self`` 
        | ``_parent`` 
        | ``_top`` 
        | ``framename`` 
        
        [<RequireQualifiedAccess>]
        type ``max`` = 
        | ``number`` 
        | ``date`` 
        
        [<RequireQualifiedAccess>]
        type ``min`` = 
        | ``number`` 
        | ``date`` 
        
        [<RequireQualifiedAccess>]
        type ``step`` = 
        | ``number`` 
        | ``any`` 
        
        [<RequireQualifiedAccess>]
        type ``type`` = 
        | ``button`` 
        | ``checkbox`` 
        | ``color`` 
        | ``date`` 
        | ``datetime-local`` 
        | ``email`` 
        | ``file`` 
        | ``hidden`` 
        | ``image`` 
        | ``month`` 
        | ``number`` 
        | ``password`` 
        | ``radio`` 
        | ``range`` 
        | ``reset`` 
        | ``search`` 
        | ``submit`` 
        | ``tel`` 
        | ``text`` 
        | ``time`` 
        | ``url`` 
        | ``week`` 
        
    
    module link =
        
        [<RequireQualifiedAccess>]
        type ``media`` = 
        | ``and`` 
        | ``not`` 
        | ``,`` 
        
        [<RequireQualifiedAccess>]
        type ``referrerpolicy`` = 
        | ``no-referrer`` 
        | ``no-referrer-when-downgrade`` 
        | ``origin`` 
        | ``origin-when-cross-origin`` 
        | ``same-origin`` 
        | ``strict-origin`` 
        | ``strict-origin-when-cross-origin`` 
        | ``unsafe-url`` 
        
        [<RequireQualifiedAccess>]
        type ``rel`` = 
        | ``alternate`` 
        | ``author`` 
        | ``dns-prefetch`` 
        | ``help`` 
        | ``icon`` 
        | ``license`` 
        | ``next`` 
        | ``pingback`` 
        | ``preconnect`` 
        | ``prefetch`` 
        | ``preload`` 
        | ``prerender`` 
        | ``prev`` 
        | ``search`` 
        | ``stylesheet`` 
        
        [<RequireQualifiedAccess>]
        type ``sizes`` = 
        | ``HeightxWidth`` 
        | ``any`` 
        
    
    module meta =
        
        [<RequireQualifiedAccess>]
        type ``http-equiv`` = 
        | ``content-security-policy`` 
        | ``content-type`` 
        | ``default-style`` 
        | ``refresh`` 
        
        [<RequireQualifiedAccess>]
        type ``name`` = 
        | ``application-name`` 
        | ``author`` 
        | ``description`` 
        | ``generator`` 
        | ``keywords`` 
        | ``viewport`` 
        
    
    module ol =
        
        [<RequireQualifiedAccess>]
        type ``type`` = 
        | ``1`` 
        | ``a`` 
        | ``A`` 
        | ``i`` 
        | ``I`` 
        
    
    module script =
        
        [<RequireQualifiedAccess>]
        type ``referrerpolicy`` = 
        | ``no-referrer`` 
        | ``no-referrer-when-downgrade`` 
        | ``origin`` 
        | ``origin-when-cross-origin`` 
        | ``same-origin`` 
        | ``strict-origin-when-cross-origin`` 
        | ``unsafe-url`` 
        
    
    module source =
        
        [<RequireQualifiedAccess>]
        type ``media`` = 
        | ``and`` 
        | ``not`` 
        | ``,`` 
        
    
    module style =
        
        [<RequireQualifiedAccess>]
        type ``media`` = 
        | ``and`` 
        | ``not`` 
        | ``,`` 
        
    
    module textarea =
        
        [<RequireQualifiedAccess>]
        type ``wrap`` = 
        | ``soft`` 
        | ``hard`` 
        
    
    module th =
        
        [<RequireQualifiedAccess>]
        type ``scope`` = 
        | ``col`` 
        | ``row`` 
        | ``colgroup`` 
        | ``rowgroup`` 
        
    
    module track =
        
        [<RequireQualifiedAccess>]
        type ``kind`` = 
        | ``captions`` 
        | ``chapters`` 
        | ``descriptions`` 
        | ``metadata`` 
        | ``subtitles`` 
        
    
    module video =
        
        [<RequireQualifiedAccess>]
        type ``preload`` = 
        | ``auto`` 
        | ``metadata`` 
        | ``none`` 
        
    
    module Global =
        
        [<RequireQualifiedAccess>]
        type ``contenteditable`` = 
        | ``true`` 
        | ``false`` 
        
        [<RequireQualifiedAccess>]
        type ``dir`` = 
        | ``ltr`` 
        | ``rtl`` 
        | ``auto`` 
        
        [<RequireQualifiedAccess>]
        type ``draggable`` = 
        | ``true`` 
        | ``false`` 
        | ``auto`` 
        
        [<RequireQualifiedAccess>]
        type ``spellcheck`` = 
        | ``true`` 
        | ``false`` 
        
        [<RequireQualifiedAccess>]
        type ``translate`` = 
        | ``yes`` 
        | ``no`` 


module HtmlElementBuilders =
   
    type a() =
        inherit NodeBuilder<HTMLAnchorElement>("a")
                    
    type abbr() =
        inherit NodeBuilder<HTMLElement>("abbr")
                    
    type address() =
        inherit NodeBuilder<HTMLElement>("address")
                    
    type area() =
        inherit NodeBuilder<HTMLAreaElement>("area")
                    
    type article() =
        inherit NodeBuilder<HTMLElement>("article")
                    
    type aside() =
        inherit NodeBuilder<HTMLElement>("aside")
                    
    type audio() =
        inherit NodeBuilder<HTMLAudioElement>("audio")
                    
    type b() =
        inherit NodeBuilder<HTMLElement>("b")
                    
    type base'() =
        inherit NodeBuilder<HTMLBaseElement>("base")
                    
    type bdi() =
        inherit NodeBuilder<HTMLElement>("bdi")
                    
    type bdo() =
        inherit NodeBuilder<HTMLElement>("bdo")
                    
    type blockquote() =
        inherit NodeBuilder<HTMLQuoteElement>("blockquote")
                    
    type body() =
        inherit NodeBuilder<HTMLBodyElement>("body")
                    
    type br() =
        inherit NodeBuilder<HTMLBRElement>("br")
                    
    type button() =
        inherit NodeBuilder<HTMLButtonElement>("button")
                    
    type canvas() =
        inherit NodeBuilder<HTMLCanvasElement>("canvas")
                    
    type caption() =
        inherit NodeBuilder<HTMLTableCaptionElement>("caption")
                    
    type cite() =
        inherit NodeBuilder<HTMLElement>("cite")
                    
    type code() =
        inherit NodeBuilder<HTMLElement>("code")
                    
    type col() =
        inherit NodeBuilder<HTMLTableColElement>("col")
                    
    type colgroup() =
        inherit NodeBuilder<HTMLTableColElement>("colgroup")
                    
    type data() =
        inherit NodeBuilder<HTMLElement>("data")
                    
    type dd() =
        inherit NodeBuilder<HTMLElement>("dd")
                    
    type del() =
        inherit NodeBuilder<HTMLModElement>("del")
                    
    type details() =
        inherit NodeBuilder<HTMLElement>("details")
                    
    type dfn() =
        inherit NodeBuilder<HTMLElement>("dfn")
                    
    type dialog() =
        inherit NodeBuilder<HTMLDialogElement>("dialog")
                    
    type div() =
        inherit NodeBuilder<HTMLDivElement>("div")
                    
    type dl() =
        inherit NodeBuilder<HTMLDListElement>("dl")
                    
    type dt() =
        inherit NodeBuilder<HTMLElement>("dt")
                    
    type em() =
        inherit NodeBuilder<HTMLElement>("em")
                    
    type embed() =
        inherit NodeBuilder<HTMLEmbedElement>("embed")
                    
    type fieldset() =
        inherit NodeBuilder<HTMLFieldSetElement>("fieldset")
                    
    type figcaption() =
        inherit NodeBuilder<HTMLElement>("figcaption")
                    
    type figure() =
        inherit NodeBuilder<HTMLElement>("figure")
                    
    type footer() =
        inherit NodeBuilder<HTMLElement>("footer")
                    
    type form() =
        inherit NodeBuilder<HTMLFormElement>("form")
                    
    type h1() =
        inherit NodeBuilder<HTMLHeadingElement>("h1")
                    
    type h2() =
        inherit NodeBuilder<HTMLHeadingElement>("h2")
                    
    type h3() =
        inherit NodeBuilder<HTMLHeadingElement>("h3")
                    
    type h4() =
        inherit NodeBuilder<HTMLHeadingElement>("h4")
                    
    type h5() =
        inherit NodeBuilder<HTMLHeadingElement>("h5")
                    
    type h6() =
        inherit NodeBuilder<HTMLHeadingElement>("h6")
                    
    type head() =
        inherit NodeBuilder<HTMLHeadElement>("head")
                    
    type header() =
        inherit NodeBuilder<HTMLElement>("header")
                    
    type hr() =
        inherit NodeBuilder<HTMLHRElement>("hr")
                    
    type html() =
        inherit NodeBuilder<HTMLHtmlElement>("html")
                    
    type i() =
        inherit NodeBuilder<HTMLElement>("i")
                    
    type iframe() =
        inherit NodeBuilder<HTMLIFrameElement>("iframe")
                    
    type img() =
        inherit NodeBuilder<HTMLImageElement>("img")
                    
    type input() =
        inherit NodeBuilder<HTMLInputElement>("input")
                    
    type ins() =
        inherit NodeBuilder<HTMLModElement>("ins")
                    
    type kbd() =
        inherit NodeBuilder<HTMLElement>("kbd")
                    
    type label() =
        inherit NodeBuilder<HTMLLabelElement>("label")
                    
    type legend() =
        inherit NodeBuilder<HTMLLegendElement>("legend")
                    
    type li() =
        inherit NodeBuilder<HTMLLIElement>("li")
                    
    type link() =
        inherit NodeBuilder<HTMLLinkElement>("link")
                    
    type main() =
        inherit NodeBuilder<HTMLElement>("main")
                    
    type map() =
        inherit NodeBuilder<HTMLMapElement>("map")
                    
    type mark() =
        inherit NodeBuilder<HTMLElement>("mark")
                    
    type meta() =
        inherit NodeBuilder<HTMLMetaElement>("meta")
                    
    type meter() =
        inherit NodeBuilder<HTMLElement>("meter")
                    
    type nav() =
        inherit NodeBuilder<HTMLElement>("nav")
                    
    type noscript() =
        inherit NodeBuilder<HTMLElement>("noscript")
                    
    type object() =
        inherit NodeBuilder<HTMLObjectElement>("object")
                    
    type ol() =
        inherit NodeBuilder<HTMLOListElement>("ol")
                    
    type optgroup() =
        inherit NodeBuilder<HTMLOptGroupElement>("optgroup")
                    
    type p() =
        inherit NodeBuilder<HTMLParagraphElement>("p")
                    
    type param() =
        inherit NodeBuilder<HTMLParamElement>("param")
                    
    type picture() =
        inherit NodeBuilder<HTMLElement>("picture")
                    
    type pre() =
        inherit NodeBuilder<HTMLPreElement>("pre")
                    
    type progress() =
        inherit NodeBuilder<HTMLProgressElement>("progress")
                    
    type q() =
        inherit NodeBuilder<HTMLQuoteElement>("q")
                    
    type rp() =
        inherit NodeBuilder<HTMLElement>("rp")
                    
    type rt() =
        inherit NodeBuilder<HTMLElement>("rt")
                    
    type ruby() =
        inherit NodeBuilder<HTMLElement>("ruby")
                    
    type s() =
        inherit NodeBuilder<HTMLElement>("s")
                    
    type samp() =
        inherit NodeBuilder<HTMLElement>("samp")
                    
    type script() =
        inherit NodeBuilder<HTMLScriptElement>("script")
                    
    type section() =
        inherit NodeBuilder<HTMLElement>("section")
                    
    type slot() =
        inherit NodeBuilder<HTMLElement>("slot")
                    
    type small() =
        inherit NodeBuilder<HTMLElement>("small")
                    
    type source() =
        inherit NodeBuilder<HTMLSourceElement>("source")
                    
    type span() =
        inherit NodeBuilder<HTMLSpanElement>("span")
                    
    type strong() =
        inherit NodeBuilder<HTMLElement>("strong")
                    
    type style() =
        inherit NodeBuilder<HTMLStyleElement>("style")
                    
    type sub() =
        inherit NodeBuilder<HTMLElement>("sub")
                    
    type summary() =
        inherit NodeBuilder<HTMLElement>("summary")
                    
    type sup() =
        inherit NodeBuilder<HTMLElement>("sup")
                    
    type table() =
        inherit NodeBuilder<HTMLTableElement>("table")
                    
    type tbody() =
        inherit NodeBuilder<HTMLTableSectionElement>("tbody")
                    
    type td() =
        inherit NodeBuilder<HTMLTableCellElement>("td")
                    
    type template() =
        inherit NodeBuilder<HTMLElement>("template")
                    
    type textarea() =
        inherit NodeBuilder<HTMLTextAreaElement>("textarea")
                    
    type tfoot() =
        inherit NodeBuilder<HTMLTableSectionElement>("tfoot")
                    
    type th() =
        inherit NodeBuilder<HTMLTableCellElement>("th")
                    
    type thead() =
        inherit NodeBuilder<HTMLTableSectionElement>("thead")
                    
    type time() =
        inherit NodeBuilder<HTMLElement>("time")
                    
    type title() =
        inherit NodeBuilder<HTMLTitleElement>("title")
                    
    type tr() =
        inherit NodeBuilder<HTMLTableRowElement>("tr")
                    
    type track() =
        inherit NodeBuilder<HTMLTrackElement>("track")
                    
    type u() =
        inherit NodeBuilder<HTMLElement>("u")
                    
    type ul() =
        inherit NodeBuilder<HTMLUListElement>("ul")
                    
    type var() =
        inherit NodeBuilder<HTMLElement>("var")
                    
    type video() =
        inherit NodeBuilder<HTMLVideoElement>("video")
                    
    type wbr() =
        inherit NodeBuilder<HTMLElement>("wbr")
                    

open HtmlElementBuilders

[<Extension>]
type NodeBuilderExtensions =
    class
        // Attributes
        
        /// Specifies a shortcut key to activate/focus an element
        [<Extension>]
        static member accesskey<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("accesskey", value))
        
        /// Specifies one or more classnames for an element (refers to a class in a style sheet)
        [<Extension>]
        static member class'<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("class", value))
        
        /// Specifies whether the content of an element is editable or not
        [<Extension>]
        static member contenteditable<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("contenteditable", value))
        
        /// Specifies whether the content of an element is editable or not
        [<Extension>]
        static member contenteditable<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: Global.``contenteditable``) =
            this.onEval(fun x -> x.node.setAttribute("contenteditable", value.ToString()))
        
        /// Specifies the text direction for the content in an element
        [<Extension>]
        static member dir<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("dir", value))
        
        /// Specifies the text direction for the content in an element
        [<Extension>]
        static member dir<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: Global.``dir``) =
            this.onEval(fun x -> x.node.setAttribute("dir", value.ToString()))
        
        /// Specifies whether an element is draggable or not
        [<Extension>]
        static member draggable<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("draggable", value))
        
        /// Specifies whether an element is draggable or not
        [<Extension>]
        static member draggable<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: Global.``draggable``) =
            this.onEval(fun x -> x.node.setAttribute("draggable", value.ToString()))
        
        /// Specifies that an element is not yet, or is no longer, relevant
        [<Extension>]
        static member hidden<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("hidden", null) else x.node.removeAttribute("hidden") )
        
        /// Specifies that an element is not yet, or is no longer, relevant
        [<Extension>]
        static member hidden<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("hidden", value))
        
        /// Specifies a unique id for an element
        [<Extension>]
        static member id<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("id", value))
        
        /// Specifies the language of the element's content
        [<Extension>]
        static member lang<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("lang", value))
        
        /// Specifies whether the element is to have its spelling and grammar checked or not
        [<Extension>]
        static member spellcheck<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("spellcheck", value))
        
        /// Specifies whether the element is to have its spelling and grammar checked or not
        [<Extension>]
        static member spellcheck<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: Global.``spellcheck``) =
            this.onEval(fun x -> x.node.setAttribute("spellcheck", value.ToString()))
        
        /// Specifies an inline CSS style for an element
        [<Extension>]
        static member style<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("style", value))
        
        /// Specifies the tabbing order of an element
        [<Extension>]
        static member tabindex<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("tabindex", value))
        
        /// Specifies extra information about an element
        [<Extension>]
        static member title<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("title", value))
        
        /// Specifies whether the content of an element should be translated or not
        [<Extension>]
        static member translate<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("translate", value))
        
        /// Specifies whether the content of an element should be translated or not
        [<Extension>]
        static member translate<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: Global.``translate``) =
            this.onEval(fun x -> x.node.setAttribute("translate", value.ToString()))
        
        /// The slot global attribute assigns a slot in a shadow DOM shadow tree to an element: An element with a slot attribute is assigned to the slot created by the slot element whose name attribute's value matches that slot attribute's value.
        [<Extension>]
        static member slot<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, value: string) =
            this.onEval(fun x -> x.node.setAttribute("slot", value))
        

        // Events
        
        /// Fires the moment that the element loses focus
        [<Extension>]
        static member onblur<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onblur <- Event.handle x.node x.app handler)

        /// Fires the moment that the element loses focus
        [<Extension>]
        static member onblur<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onblur <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires the moment when the value of the element is changed
        [<Extension>]
        static member onchange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onchange <- Event.handle x.node x.app handler)

        /// Fires the moment when the value of the element is changed
        [<Extension>]
        static member onchange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onchange <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when a context menu is triggered
        [<Extension>]
        static member oncontextmenu<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.oncontextmenu <- Event.handle x.node x.app handler)

        /// Script to be run when a context menu is triggered
        [<Extension>]
        static member oncontextmenu<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.oncontextmenu <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires the moment when the element gets focus
        [<Extension>]
        static member onfocus<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onfocus <- Event.handle x.node x.app handler)

        /// Fires the moment when the element gets focus
        [<Extension>]
        static member onfocus<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onfocus <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when an element gets user input
        [<Extension>]
        static member oninput<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.oninput <- Event.handle x.node x.app handler)

        /// Script to be run when an element gets user input
        [<Extension>]
        static member oninput<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.oninput <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the Reset button in a form is clicked
        [<Extension>]
        static member onreset<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onreset <- Event.handle x.node x.app handler)

        /// Fires when the Reset button in a form is clicked
        [<Extension>]
        static member onreset<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onreset <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires after some text has been selected in an element
        [<Extension>]
        static member onselect<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onselect <- Event.handle x.node x.app handler)

        /// Fires after some text has been selected in an element
        [<Extension>]
        static member onselect<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onselect <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when a form is submitted
        [<Extension>]
        static member onsubmit<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onsubmit <- Event.handle x.node x.app handler)

        /// Fires when a form is submitted
        [<Extension>]
        static member onsubmit<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onsubmit <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when a user is pressing a key
        [<Extension>]
        static member onkeydown<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onkeydown <- Event.handle x.node x.app handler)

        /// Fires when a user is pressing a key
        [<Extension>]
        static member onkeydown<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onkeydown <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when a user presses a key
        [<Extension>]
        static member onkeypress<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onkeypress <- Event.handle x.node x.app handler)

        /// Fires when a user presses a key
        [<Extension>]
        static member onkeypress<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onkeypress <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when a user releases a key
        [<Extension>]
        static member onkeyup<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onkeyup <- Event.handle x.node x.app handler)

        /// Fires when a user releases a key
        [<Extension>]
        static member onkeyup<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onkeyup <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires on a mouse click on the element
        [<Extension>]
        static member onclick<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onclick <- Event.handle x.node x.app handler)

        /// Fires on a mouse click on the element
        [<Extension>]
        static member onclick<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onclick <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires on a mouse double-click on the element
        [<Extension>]
        static member ondblclick<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondblclick <- Event.handle x.node x.app handler)

        /// Fires on a mouse double-click on the element
        [<Extension>]
        static member ondblclick<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondblclick <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when a mouse button is pressed down on an element
        [<Extension>]
        static member onmousedown<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onmousedown <- Event.handle x.node x.app handler)

        /// Fires when a mouse button is pressed down on an element
        [<Extension>]
        static member onmousedown<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onmousedown <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the mouse pointer is moving while it is over an element
        [<Extension>]
        static member onmousemove<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onmousemove <- Event.handle x.node x.app handler)

        /// Fires when the mouse pointer is moving while it is over an element
        [<Extension>]
        static member onmousemove<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onmousemove <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the mouse pointer moves out of an element
        [<Extension>]
        static member onmouseout<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onmouseout <- Event.handle x.node x.app handler)

        /// Fires when the mouse pointer moves out of an element
        [<Extension>]
        static member onmouseout<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onmouseout <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the mouse pointer moves over an element
        [<Extension>]
        static member onmouseover<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onmouseover <- Event.handle x.node x.app handler)

        /// Fires when the mouse pointer moves over an element
        [<Extension>]
        static member onmouseover<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onmouseover <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when a mouse button is released over an element
        [<Extension>]
        static member onmouseup<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onmouseup <- Event.handle x.node x.app handler)

        /// Fires when a mouse button is released over an element
        [<Extension>]
        static member onmouseup<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onmouseup <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Deprecated. Use the onwheel attribute instead
        [<Extension>]
        static member onmousewheel<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onmousewheel <- Event.handle x.node x.app handler)

        /// Deprecated. Use the onwheel attribute instead
        [<Extension>]
        static member onmousewheel<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onmousewheel <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the mouse wheel rolls up or down over an element
        [<Extension>]
        static member onwheel<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onwheel <- Event.handle x.node x.app handler)

        /// Fires when the mouse wheel rolls up or down over an element
        [<Extension>]
        static member onwheel<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onwheel <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when an element is dragged
        [<Extension>]
        static member ondrag<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondrag <- Event.handle x.node x.app handler)

        /// Script to be run when an element is dragged
        [<Extension>]
        static member ondrag<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondrag <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run at the end of a drag operation
        [<Extension>]
        static member ondragend<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondragend <- Event.handle x.node x.app handler)

        /// Script to be run at the end of a drag operation
        [<Extension>]
        static member ondragend<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondragend <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when an element has been dragged to a valid drop target
        [<Extension>]
        static member ondragenter<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondragenter <- Event.handle x.node x.app handler)

        /// Script to be run when an element has been dragged to a valid drop target
        [<Extension>]
        static member ondragenter<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondragenter <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when an element leaves a valid drop target
        [<Extension>]
        static member ondragleave<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondragleave <- Event.handle x.node x.app handler)

        /// Script to be run when an element leaves a valid drop target
        [<Extension>]
        static member ondragleave<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondragleave <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when an element is being dragged over a valid drop target
        [<Extension>]
        static member ondragover<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondragover <- Event.handle x.node x.app handler)

        /// Script to be run when an element is being dragged over a valid drop target
        [<Extension>]
        static member ondragover<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondragover <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run at the start of a drag operation
        [<Extension>]
        static member ondragstart<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondragstart <- Event.handle x.node x.app handler)

        /// Script to be run at the start of a drag operation
        [<Extension>]
        static member ondragstart<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondragstart <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when dragged element is being dropped
        [<Extension>]
        static member ondrop<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondrop <- Event.handle x.node x.app handler)

        /// Script to be run when dragged element is being dropped
        [<Extension>]
        static member ondrop<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondrop <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when an element's scrollbar is being scrolled
        [<Extension>]
        static member onscroll<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onscroll <- Event.handle x.node x.app handler)

        /// Script to be run when an element's scrollbar is being scrolled
        [<Extension>]
        static member onscroll<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onscroll <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the user copies the content of an element
        [<Extension>]
        static member oncopy<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.oncopy <- Event.handle x.node x.app handler)

        /// Fires when the user copies the content of an element
        [<Extension>]
        static member oncopy<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.oncopy <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the user cuts the content of an element
        [<Extension>]
        static member oncut<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.oncut <- Event.handle x.node x.app handler)

        /// Fires when the user cuts the content of an element
        [<Extension>]
        static member oncut<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.oncut <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Fires when the user pastes some content in an element
        [<Extension>]
        static member onpaste<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onpaste <- Event.handle x.node x.app handler)

        /// Fires when the user pastes some content in an element
        [<Extension>]
        static member onpaste<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onpaste <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run on abort
        [<Extension>]
        static member onabort<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onabort <- Event.handle x.node x.app handler)

        /// Script to be run on abort
        [<Extension>]
        static member onabort<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onabort <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when a file is ready to start playing (when it has buffered enough to begin)
        [<Extension>]
        static member oncanplay<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.oncanplay <- Event.handle x.node x.app handler)

        /// Script to be run when a file is ready to start playing (when it has buffered enough to begin)
        [<Extension>]
        static member oncanplay<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.oncanplay <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when a file can be played all the way to the end without pausing for buffering
        [<Extension>]
        static member oncanplaythrough<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.oncanplaythrough <- Event.handle x.node x.app handler)

        /// Script to be run when a file can be played all the way to the end without pausing for buffering
        [<Extension>]
        static member oncanplaythrough<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.oncanplaythrough <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the cue changes in a <track> element
        [<Extension>]
        static member oncuechange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.oncuechange <- Event.handle x.node x.app handler)

        /// Script to be run when the cue changes in a <track> element
        [<Extension>]
        static member oncuechange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.oncuechange <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the length of the media changes
        [<Extension>]
        static member ondurationchange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ondurationchange <- Event.handle x.node x.app handler)

        /// Script to be run when the length of the media changes
        [<Extension>]
        static member ondurationchange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ondurationchange <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when something bad happens and the file is suddenly unavailable (like unexpectedly disconnects)
        [<Extension>]
        static member onemptied<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onemptied <- Event.handle x.node x.app handler)

        /// Script to be run when something bad happens and the file is suddenly unavailable (like unexpectedly disconnects)
        [<Extension>]
        static member onemptied<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onemptied <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the media has reach the end (a useful event for messages like thanks for listening)
        [<Extension>]
        static member onended<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onended <- Event.handle x.node x.app handler)

        /// Script to be run when the media has reach the end (a useful event for messages like thanks for listening)
        [<Extension>]
        static member onended<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onended <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when an error occurs when the file is being loaded
        [<Extension>]
        static member onerror<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onerror <- Event.handle x.node x.app handler)

        /// Script to be run when an error occurs when the file is being loaded
        [<Extension>]
        static member onerror<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onerror <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when media data is loaded
        [<Extension>]
        static member onloadeddata<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onloadeddata <- Event.handle x.node x.app handler)

        /// Script to be run when media data is loaded
        [<Extension>]
        static member onloadeddata<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onloadeddata <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when meta data (like dimensions and duration) are loaded
        [<Extension>]
        static member onloadedmetadata<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onloadedmetadata <- Event.handle x.node x.app handler)

        /// Script to be run when meta data (like dimensions and duration) are loaded
        [<Extension>]
        static member onloadedmetadata<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onloadedmetadata <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run just as the file begins to load before anything is actually loaded
        [<Extension>]
        static member onloadstart<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onloadstart <- Event.handle x.node x.app handler)

        /// Script to be run just as the file begins to load before anything is actually loaded
        [<Extension>]
        static member onloadstart<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onloadstart <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the media is paused either by the user or programmatically
        [<Extension>]
        static member onpause<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onpause <- Event.handle x.node x.app handler)

        /// Script to be run when the media is paused either by the user or programmatically
        [<Extension>]
        static member onpause<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onpause <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the media is ready to start playing
        [<Extension>]
        static member onplay<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onplay <- Event.handle x.node x.app handler)

        /// Script to be run when the media is ready to start playing
        [<Extension>]
        static member onplay<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onplay <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the media actually has started playing
        [<Extension>]
        static member onplaying<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onplaying <- Event.handle x.node x.app handler)

        /// Script to be run when the media actually has started playing
        [<Extension>]
        static member onplaying<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onplaying <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the browser is in the process of getting the media data
        [<Extension>]
        static member onprogress<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onprogress <- Event.handle x.node x.app handler)

        /// Script to be run when the browser is in the process of getting the media data
        [<Extension>]
        static member onprogress<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onprogress <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run each time the playback rate changes (like when a user switches to a slow motion or fast forward mode)
        [<Extension>]
        static member onratechange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onratechange <- Event.handle x.node x.app handler)

        /// Script to be run each time the playback rate changes (like when a user switches to a slow motion or fast forward mode)
        [<Extension>]
        static member onratechange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onratechange <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the seeking attribute is set to false indicating that seeking has ended
        [<Extension>]
        static member onseeked<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onseeked <- Event.handle x.node x.app handler)

        /// Script to be run when the seeking attribute is set to false indicating that seeking has ended
        [<Extension>]
        static member onseeked<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onseeked <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the seeking attribute is set to true indicating that seeking is active
        [<Extension>]
        static member onseeking<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onseeking <- Event.handle x.node x.app handler)

        /// Script to be run when the seeking attribute is set to true indicating that seeking is active
        [<Extension>]
        static member onseeking<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onseeking <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the browser is unable to fetch the media data for whatever reason
        [<Extension>]
        static member onstalled<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onstalled <- Event.handle x.node x.app handler)

        /// Script to be run when the browser is unable to fetch the media data for whatever reason
        [<Extension>]
        static member onstalled<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onstalled <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when fetching the media data is stopped before it is completely loaded for whatever reason
        [<Extension>]
        static member onsuspend<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onsuspend <- Event.handle x.node x.app handler)

        /// Script to be run when fetching the media data is stopped before it is completely loaded for whatever reason
        [<Extension>]
        static member onsuspend<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onsuspend <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the playing position has changed (like when the user fast forwards to a different point in the media)
        [<Extension>]
        static member ontimeupdate<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.ontimeupdate <- Event.handle x.node x.app handler)

        /// Script to be run when the playing position has changed (like when the user fast forwards to a different point in the media)
        [<Extension>]
        static member ontimeupdate<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.ontimeupdate <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run each time the volume is changed which (includes setting the volume to mute)
        [<Extension>]
        static member onvolumechange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onvolumechange <- Event.handle x.node x.app handler)

        /// Script to be run each time the volume is changed which (includes setting the volume to mute)
        [<Extension>]
        static member onvolumechange<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onvolumechange <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
        /// Script to be run when the media has paused but is expected to resume (like when the media pauses to buffer more data)
        [<Extension>]
        static member onwaiting<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, handler) =
            this.onEval(fun x -> x.node.onwaiting <- Event.handle x.node x.app handler)

        /// Script to be run when the media has paused but is expected to resume (like when the media pauses to buffer more data)
        [<Extension>]
        static member onwaiting<'nb,'e when 'nb :> NodeBuilder<'e> and 'e :> HTMLElement>(this: 'nb, ?requestEvaluation: bool) =
            this.onEval(fun x -> x.node.onwaiting <- Event.handle x.node x.app (fun args ->
                args.requestEvaluation <- defaultArg requestEvaluation true))
        
    end


[<Extension>]
type aExtensions =
    class
        // Attributes
        
        /// Specifies that the target will be downloaded when a user clicks on the hyperlink
        [<Extension>]
        static member download(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("download", value))
        
        /// Specifies the URL of the page the link goes to
        [<Extension>]
        static member href(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("href", value))
        
        /// Specifies the language of the linked document
        [<Extension>]
        static member hreflang(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("hreflang", value))
        
        /// Specifies what media/device the linked document is optimized for
        [<Extension>]
        static member media(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("media", value))
        
        /// Specifies what media/device the linked document is optimized for
        [<Extension>]
        static member media(this: #a, value: a.``media``) =
            this.onEval(fun x -> x.node.setAttribute("media", value.ToString()))
        
        /// Specifies a space-separated list of URLs to which, when the link is followed, post requests with the body ping will be sent by the browser (in the background). Typically used for tracking.
        [<Extension>]
        static member ping(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("ping", value))
        
        /// Specifies which referrer information to send with the link
        [<Extension>]
        static member referrerpolicy(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value))
        
        /// Specifies which referrer information to send with the link
        [<Extension>]
        static member referrerpolicy(this: #a, value: a.``referrerpolicy``) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value.ToString()))
        
        /// Specifies the relationship between the current document and the linked document
        [<Extension>]
        static member rel(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("rel", value))
        
        /// Specifies the relationship between the current document and the linked document
        [<Extension>]
        static member rel(this: #a, value: a.``rel``) =
            this.onEval(fun x -> x.node.setAttribute("rel", value.ToString()))
        
        /// Specifies where to open the linked document
        [<Extension>]
        static member target(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("target", value))
        
        /// Specifies where to open the linked document
        [<Extension>]
        static member target(this: #a, value: a.``target``) =
            this.onEval(fun x -> x.node.setAttribute("target", value.ToString()))
        
        /// Specifies the media type of the linked document
        [<Extension>]
        static member type'(this: #a, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        

        // Events
        
    end

[<Extension>]
type abbrExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type addressExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type areaExtensions =
    class
        // Attributes
        
        /// Specifies an alternate text for the area. Required if the href attribute is present
        [<Extension>]
        static member alt(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("alt", value))
        
        /// Specifies the coordinates of the area
        [<Extension>]
        static member coords(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("coords", value))
        
        /// Specifies the coordinates of the area
        [<Extension>]
        static member coords(this: #area, value: area.``coords``) =
            this.onEval(fun x -> x.node.setAttribute("coords", value.ToString()))
        
        /// Specifies that the target will be downloaded when a user clicks on the hyperlink
        [<Extension>]
        static member download(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("download", value))
        
        /// Specifies the hyperlink target for the area
        [<Extension>]
        static member href(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("href", value))
        
        /// Specifies the language of the target URL
        [<Extension>]
        static member hreflang(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("hreflang", value))
        
        /// Specifies what media/device the target URL is optimized for
        [<Extension>]
        static member media(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("media", value))
        
        /// Specifies what media/device the target URL is optimized for
        [<Extension>]
        static member media(this: #area, value: area.``media``) =
            this.onEval(fun x -> x.node.setAttribute("media", value.ToString()))
        
        /// Specifies which referrer information to send with the link
        [<Extension>]
        static member referrerpolicy(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value))
        
        /// Specifies which referrer information to send with the link
        [<Extension>]
        static member referrerpolicy(this: #area, value: area.``referrerpolicy``) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value.ToString()))
        
        /// Specifies the relationship between the current document and the target URL
        [<Extension>]
        static member rel(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("rel", value))
        
        /// Specifies the relationship between the current document and the target URL
        [<Extension>]
        static member rel(this: #area, value: area.``rel``) =
            this.onEval(fun x -> x.node.setAttribute("rel", value.ToString()))
        
        /// Specifies the shape of the area
        [<Extension>]
        static member shape(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("shape", value))
        
        /// Specifies the shape of the area
        [<Extension>]
        static member shape(this: #area, value: area.``shape``) =
            this.onEval(fun x -> x.node.setAttribute("shape", value.ToString()))
        
        /// Specifies where to open the target URL
        [<Extension>]
        static member target(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("target", value))
        
        /// Specifies where to open the target URL
        [<Extension>]
        static member target(this: #area, value: area.``target``) =
            this.onEval(fun x -> x.node.setAttribute("target", value.ToString()))
        
        /// Specifies the media type of the target URL
        [<Extension>]
        static member type'(this: #area, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        

        // Events
        
    end

[<Extension>]
type articleExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type asideExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type audioExtensions =
    class
        // Attributes
        
        /// Specifies that the audio will start playing as soon as it is ready
        [<Extension>]
        static member autoplay(this: #audio, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("autoplay", null) else x.node.removeAttribute("autoplay") )
        
        /// Specifies that the audio will start playing as soon as it is ready
        [<Extension>]
        static member autoplay(this: #audio, value: string) =
            this.onEval(fun x -> x.node.setAttribute("autoplay", value))
        
        /// Specifies that audio controls should be displayed (such as a play/pause button etc)
        [<Extension>]
        static member controls(this: #audio, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("controls", null) else x.node.removeAttribute("controls") )
        
        /// Specifies that audio controls should be displayed (such as a play/pause button etc)
        [<Extension>]
        static member controls(this: #audio, value: string) =
            this.onEval(fun x -> x.node.setAttribute("controls", value))
        
        /// Specifies that the audio will start over again, every time it is finished
        [<Extension>]
        static member loop(this: #audio, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("loop", null) else x.node.removeAttribute("loop") )
        
        /// Specifies that the audio will start over again, every time it is finished
        [<Extension>]
        static member loop(this: #audio, value: string) =
            this.onEval(fun x -> x.node.setAttribute("loop", value))
        
        /// Specifies that the audio output should be muted
        [<Extension>]
        static member muted(this: #audio, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("muted", null) else x.node.removeAttribute("muted") )
        
        /// Specifies that the audio output should be muted
        [<Extension>]
        static member muted(this: #audio, value: string) =
            this.onEval(fun x -> x.node.setAttribute("muted", value))
        
        /// Specifies if and how the author thinks the audio should be loaded when the page loads
        [<Extension>]
        static member preload(this: #audio, value: string) =
            this.onEval(fun x -> x.node.setAttribute("preload", value))
        
        /// Specifies if and how the author thinks the audio should be loaded when the page loads
        [<Extension>]
        static member preload(this: #audio, value: audio.``preload``) =
            this.onEval(fun x -> x.node.setAttribute("preload", value.ToString()))
        
        /// Specifies the URL of the audio file
        [<Extension>]
        static member src(this: #audio, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        

        // Events
        
    end

[<Extension>]
type bExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type base'Extensions =
    class
        // Attributes
        
        /// Specifies the base URL for all relative URLs in the page
        [<Extension>]
        static member href(this: #base', value: string) =
            this.onEval(fun x -> x.node.setAttribute("href", value))
        
        /// Specifies the default target for all hyperlinks and forms in the page
        [<Extension>]
        static member target(this: #base', value: string) =
            this.onEval(fun x -> x.node.setAttribute("target", value))
        
        /// Specifies the default target for all hyperlinks and forms in the page
        [<Extension>]
        static member target(this: #base', value: base'.``target``) =
            this.onEval(fun x -> x.node.setAttribute("target", value.ToString()))
        

        // Events
        
    end

[<Extension>]
type bdiExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type bdoExtensions =
    class
        // Attributes
        
        /// Required. Specifies the text direction of the text inside the <bdo> element
        [<Extension>]
        static member dir(this: #bdo, value: string) =
            this.onEval(fun x -> x.node.setAttribute("dir", value))
        
        /// Required. Specifies the text direction of the text inside the <bdo> element
        [<Extension>]
        static member dir(this: #bdo, value: bdo.``dir``) =
            this.onEval(fun x -> x.node.setAttribute("dir", value.ToString()))
        

        // Events
        
    end

[<Extension>]
type blockquoteExtensions =
    class
        // Attributes
        
        /// Specifies the source of the quotation
        [<Extension>]
        static member cite(this: #blockquote, value: string) =
            this.onEval(fun x -> x.node.setAttribute("cite", value))
        

        // Events
        
    end

[<Extension>]
type bodyExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type brExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type buttonExtensions =
    class
        // Attributes
        
        /// Specifies that a button should automatically get focus when the page loads
        [<Extension>]
        static member autofocus(this: #button, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("autofocus", null) else x.node.removeAttribute("autofocus") )
        
        /// Specifies that a button should automatically get focus when the page loads
        [<Extension>]
        static member autofocus(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("autofocus", value))
        
        /// Specifies that a button should be disabled
        [<Extension>]
        static member disabled(this: #button, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("disabled", null) else x.node.removeAttribute("disabled") )
        
        /// Specifies that a button should be disabled
        [<Extension>]
        static member disabled(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("disabled", value))
        
        /// Specifies which form the button belongs to
        [<Extension>]
        static member form(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("form", value))
        
        /// Specifies where to send the form-data when a form is submitted. Only for type="submit"
        [<Extension>]
        static member formaction(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formaction", value))
        
        /// Specifies how form-data should be encoded before sending it to a server. Only for type="submit"
        [<Extension>]
        static member formenctype(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formenctype", value))
        
        /// Specifies how form-data should be encoded before sending it to a server. Only for type="submit"
        [<Extension>]
        static member formenctype(this: #button, value: button.``formenctype``) =
            this.onEval(fun x -> x.node.setAttribute("formenctype", value.ToString()))
        
        /// Specifies how to send the form-data (which HTTP method to use). Only for type="submit"
        [<Extension>]
        static member formmethod(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formmethod", value))
        
        /// Specifies how to send the form-data (which HTTP method to use). Only for type="submit"
        [<Extension>]
        static member formmethod(this: #button, value: button.``formmethod``) =
            this.onEval(fun x -> x.node.setAttribute("formmethod", value.ToString()))
        
        /// Specifies that the form-data should not be validated on submission. Only for type="submit"
        [<Extension>]
        static member formnovalidate(this: #button, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("formnovalidate", null) else x.node.removeAttribute("formnovalidate") )
        
        /// Specifies that the form-data should not be validated on submission. Only for type="submit"
        [<Extension>]
        static member formnovalidate(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formnovalidate", value))
        
        /// Specifies where to display the response after submitting the form. Only for type="submit"
        [<Extension>]
        static member formtarget(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formtarget", value))
        
        /// Specifies where to display the response after submitting the form. Only for type="submit"
        [<Extension>]
        static member formtarget(this: #button, value: button.``formtarget``) =
            this.onEval(fun x -> x.node.setAttribute("formtarget", value.ToString()))
        
        /// Specifies a name for the button
        [<Extension>]
        static member name(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies the type of button
        [<Extension>]
        static member type'(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        
        /// Specifies the type of button
        [<Extension>]
        static member type'(this: #button, value: button.``type``) =
            this.onEval(fun x -> x.node.setAttribute("type", value.ToString()))
        
        /// Specifies an initial value for the button
        [<Extension>]
        static member value(this: #button, value: string) =
            this.onEval(fun x -> x.node.setAttribute("value", value))
        

        // Events
        
    end

[<Extension>]
type canvasExtensions =
    class
        // Attributes
        
        /// Specifies the height of the canvas. Default value is 150
        [<Extension>]
        static member height(this: #canvas, value: string) =
            this.onEval(fun x -> x.node.setAttribute("height", value))
        
        /// Specifies the width of the canvas Default value is 300
        [<Extension>]
        static member width(this: #canvas, value: string) =
            this.onEval(fun x -> x.node.setAttribute("width", value))
        

        // Events
        
    end

[<Extension>]
type captionExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type citeExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type codeExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type colExtensions =
    class
        // Attributes
        
        /// Specifies the number of columns a <col> element should span
        [<Extension>]
        static member span(this: #col, value: string) =
            this.onEval(fun x -> x.node.setAttribute("span", value))
        

        // Events
        
    end

[<Extension>]
type colgroupExtensions =
    class
        // Attributes
        
        /// Specifies the number of columns a column group should span
        [<Extension>]
        static member span(this: #colgroup, value: string) =
            this.onEval(fun x -> x.node.setAttribute("span", value))
        

        // Events
        
    end

[<Extension>]
type dataExtensions =
    class
        // Attributes
        
        /// Specifies the machine-readable translation of the content of the element
        [<Extension>]
        static member value(this: #data, value: string) =
            this.onEval(fun x -> x.node.setAttribute("value", value))
        

        // Events
        
    end

[<Extension>]
type datalistExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type ddExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type delExtensions =
    class
        // Attributes
        
        /// Specifies a URL to a document that explains the reason why the text was deleted/changed
        [<Extension>]
        static member cite(this: #del, value: string) =
            this.onEval(fun x -> x.node.setAttribute("cite", value))
        
        /// Specifies the date and time of when the text was deleted/changed
        [<Extension>]
        static member datetime(this: #del, value: string) =
            this.onEval(fun x -> x.node.setAttribute("datetime", value))
        

        // Events
        
    end

[<Extension>]
type detailsExtensions =
    class
        // Attributes
        
        /// Specifies that the details should be visible (open) to the user
        [<Extension>]
        static member open'(this: #details, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("open", null) else x.node.removeAttribute("open") )
        
        /// Specifies that the details should be visible (open) to the user
        [<Extension>]
        static member open'(this: #details, value: string) =
            this.onEval(fun x -> x.node.setAttribute("open", value))
        

        // Events
        
    end

[<Extension>]
type dfnExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type dialogExtensions =
    class
        // Attributes
        
        /// Specifies that the dialog element is active and that the user can interact with it
        [<Extension>]
        static member open'(this: #dialog, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("open", null) else x.node.removeAttribute("open") )
        
        /// Specifies that the dialog element is active and that the user can interact with it
        [<Extension>]
        static member open'(this: #dialog, value: string) =
            this.onEval(fun x -> x.node.setAttribute("open", value))
        

        // Events
        
    end

[<Extension>]
type divExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type dlExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type dtExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type emExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type embedExtensions =
    class
        // Attributes
        
        /// Specifies the height of the embedded content
        [<Extension>]
        static member height(this: #embed, value: string) =
            this.onEval(fun x -> x.node.setAttribute("height", value))
        
        /// Specifies the address of the external file to embed
        [<Extension>]
        static member src(this: #embed, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Specifies the media type of the embedded content
        [<Extension>]
        static member type'(this: #embed, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        
        /// Specifies the width of the embedded content
        [<Extension>]
        static member width(this: #embed, value: string) =
            this.onEval(fun x -> x.node.setAttribute("width", value))
        

        // Events
        
    end

[<Extension>]
type fieldsetExtensions =
    class
        // Attributes
        
        /// Specifies that a group of related form elements should be disabled
        [<Extension>]
        static member disabled(this: #fieldset, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("disabled", null) else x.node.removeAttribute("disabled") )
        
        /// Specifies that a group of related form elements should be disabled
        [<Extension>]
        static member disabled(this: #fieldset, value: string) =
            this.onEval(fun x -> x.node.setAttribute("disabled", value))
        
        /// Specifies which form the fieldset belongs to
        [<Extension>]
        static member form(this: #fieldset, value: string) =
            this.onEval(fun x -> x.node.setAttribute("form", value))
        
        /// Specifies a name for the fieldset
        [<Extension>]
        static member name(this: #fieldset, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        

        // Events
        
    end

[<Extension>]
type figcaptionExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type figureExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type footerExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type formExtensions =
    class
        // Attributes
        
        /// Specifies the character encodings that are to be used for the form submission
        [<Extension>]
        static member acceptCharset(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("accept-charset", value))
        
        /// Specifies where to send the form-data when a form is submitted
        [<Extension>]
        static member action(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("action", value))
        
        /// Specifies whether a form should have autocomplete on or off
        [<Extension>]
        static member autocomplete(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("autocomplete", value))
        
        /// Specifies whether a form should have autocomplete on or off
        [<Extension>]
        static member autocomplete(this: #form, value: form.``autocomplete``) =
            this.onEval(fun x -> x.node.setAttribute("autocomplete", value.ToString()))
        
        /// Specifies how the form-data should be encoded when submitting it to the server (only for method="post")
        [<Extension>]
        static member enctype(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("enctype", value))
        
        /// Specifies how the form-data should be encoded when submitting it to the server (only for method="post")
        [<Extension>]
        static member enctype(this: #form, value: form.``enctype``) =
            this.onEval(fun x -> x.node.setAttribute("enctype", value.ToString()))
        
        /// Specifies the HTTP method to use when sending form-data
        [<Extension>]
        static member method(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("method", value))
        
        /// Specifies the HTTP method to use when sending form-data
        [<Extension>]
        static member method(this: #form, value: form.``method``) =
            this.onEval(fun x -> x.node.setAttribute("method", value.ToString()))
        
        /// Specifies the name of a form
        [<Extension>]
        static member name(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies that the form should not be validated when submitted
        [<Extension>]
        static member novalidate(this: #form, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("novalidate", null) else x.node.removeAttribute("novalidate") )
        
        /// Specifies that the form should not be validated when submitted
        [<Extension>]
        static member novalidate(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("novalidate", value))
        
        /// Specifies the relationship between a linked resource and the current document
        [<Extension>]
        static member rel(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("rel", value))
        
        /// Specifies the relationship between a linked resource and the current document
        [<Extension>]
        static member rel(this: #form, value: form.``rel``) =
            this.onEval(fun x -> x.node.setAttribute("rel", value.ToString()))
        
        /// Specifies where to display the response that is received after submitting the form
        [<Extension>]
        static member target(this: #form, value: string) =
            this.onEval(fun x -> x.node.setAttribute("target", value))
        
        /// Specifies where to display the response that is received after submitting the form
        [<Extension>]
        static member target(this: #form, value: form.``target``) =
            this.onEval(fun x -> x.node.setAttribute("target", value.ToString()))
        

        // Events
        
    end

[<Extension>]
type h1Extensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type h2Extensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type h3Extensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type h4Extensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type h5Extensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type h6Extensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type headExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type headerExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type hrExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type htmlExtensions =
    class
        // Attributes
        
        /// Specifies the XML namespace attribute (If you need your content to conform to XHTML)
        [<Extension>]
        static member xmlns(this: #html, value: string) =
            this.onEval(fun x -> x.node.setAttribute("xmlns", value))
        

        // Events
        
    end

[<Extension>]
type iExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type iframeExtensions =
    class
        // Attributes
        
        /// Specifies a feature policy for the <iframe>
        [<Extension>]
        static member allow(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("allow", value))
        
        /// Set to true if the <iframe> can activate fullscreen mode by calling the requestFullscreen() method
        [<Extension>]
        static member allowfullscreen(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("allowfullscreen", value))
        
        /// Set to true if a cross-origin <iframe> should be allowed to invoke the Payment Request API
        [<Extension>]
        static member allowpaymentrequest(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("allowpaymentrequest", value))
        
        /// Specifies the height of an <iframe>. Default height is 150 pixels
        [<Extension>]
        static member height(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("height", value))
        
        /// Specifies whether a browser should load an iframe immediately or to defer loading of iframes until some conditions are met
        [<Extension>]
        static member loading(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("loading", value))
        
        /// Specifies the name of an <iframe>
        [<Extension>]
        static member name(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies which referrer information to send when fetching the iframe 
        [<Extension>]
        static member referrerpolicy(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value))
        
        /// Specifies which referrer information to send when fetching the iframe 
        [<Extension>]
        static member referrerpolicy(this: #iframe, value: iframe.``referrerpolicy``) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value.ToString()))
        
        /// Enables an extra set of restrictions for the content in an <iframe>
        [<Extension>]
        static member sandbox(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("sandbox", value))
        
        /// Enables an extra set of restrictions for the content in an <iframe>
        [<Extension>]
        static member sandbox(this: #iframe, value: iframe.``sandbox``) =
            this.onEval(fun x -> x.node.setAttribute("sandbox", value.ToString()))
        
        /// Specifies the address of the document to embed in the <iframe>
        [<Extension>]
        static member src(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Specifies the HTML content of the page to show in the <iframe>
        [<Extension>]
        static member srcdoc(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("srcdoc", value))
        
        /// Specifies the width of an <iframe>. Default width is 300 pixels
        [<Extension>]
        static member width(this: #iframe, value: string) =
            this.onEval(fun x -> x.node.setAttribute("width", value))
        

        // Events
        
    end

[<Extension>]
type imgExtensions =
    class
        // Attributes
        
        /// Specifies an alternate text for an image
        [<Extension>]
        static member alt(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("alt", value))
        
        /// Allow images from third-party sites that allow cross-origin access to be used with canvas
        [<Extension>]
        static member crossorigin(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("crossorigin", value))
        
        /// Specifies the height of an image
        [<Extension>]
        static member height(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("height", value))
        
        /// Specifies an image as a server-side image map
        [<Extension>]
        static member ismap(this: #img, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("ismap", null) else x.node.removeAttribute("ismap") )
        
        /// Specifies an image as a server-side image map
        [<Extension>]
        static member ismap(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("ismap", value))
        
        /// Specifies whether a browser should load an image immediately or to defer loading of images until some conditions are met
        [<Extension>]
        static member loading(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("loading", value))
        
        /// Specifies whether a browser should load an image immediately or to defer loading of images until some conditions are met
        [<Extension>]
        static member loading(this: #img, value: img.``loading``) =
            this.onEval(fun x -> x.node.setAttribute("loading", value.ToString()))
        
        /// Specifies a URL to a detailed description of an image
        [<Extension>]
        static member longdesc(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("longdesc", value))
        
        /// Specifies which referrer information to use when fetching an image
        [<Extension>]
        static member referrerpolicy(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value))
        
        /// Specifies which referrer information to use when fetching an image
        [<Extension>]
        static member referrerpolicy(this: #img, value: img.``referrerpolicy``) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value.ToString()))
        
        /// Specifies image sizes for different page layouts
        [<Extension>]
        static member sizes(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("sizes", value))
        
        /// Specifies the path to the image
        [<Extension>]
        static member src(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Specifies a list of image files to use in different situations
        [<Extension>]
        static member srcset(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("srcset", value))
        
        /// Specifies an image as a client-side image map
        [<Extension>]
        static member usemap(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("usemap", value))
        
        /// Specifies the width of an image
        [<Extension>]
        static member width(this: #img, value: string) =
            this.onEval(fun x -> x.node.setAttribute("width", value))
        

        // Events
        
    end

[<Extension>]
type inputExtensions =
    class
        // Attributes
        
        /// Specifies a filter for what file types the user can pick from the file input dialog box (only for type="file")
        [<Extension>]
        static member accept(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("accept", value))
        
        /// Specifies a filter for what file types the user can pick from the file input dialog box (only for type="file")
        [<Extension>]
        static member accept(this: #input, value: input.``accept``) =
            this.onEval(fun x -> x.node.setAttribute("accept", value.ToString()))
        
        /// Specifies an alternate text for images (only for type="image")
        [<Extension>]
        static member alt(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("alt", value))
        
        /// Specifies whether an <input> element should have autocomplete enabled
        [<Extension>]
        static member autocomplete(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("autocomplete", value))
        
        /// Specifies whether an <input> element should have autocomplete enabled
        [<Extension>]
        static member autocomplete(this: #input, value: input.``autocomplete``) =
            this.onEval(fun x -> x.node.setAttribute("autocomplete", value.ToString()))
        
        /// Specifies that an <input> element should automatically get focus when the page loads
        [<Extension>]
        static member autofocus(this: #input, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("autofocus", null) else x.node.removeAttribute("autofocus") )
        
        /// Specifies that an <input> element should automatically get focus when the page loads
        [<Extension>]
        static member autofocus(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("autofocus", value))
        
        /// Specifies that an <input> element should be pre-selected when the page loads (for type="checkbox" or type="radio")
        [<Extension>]
        static member checked'(this: #input, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("checked", null) else x.node.removeAttribute("checked") )
        
        /// Specifies that an <input> element should be pre-selected when the page loads (for type="checkbox" or type="radio")
        [<Extension>]
        static member checked'(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("checked", value))
        
        /// Specifies that the text direction will be submitted
        [<Extension>]
        static member dirname(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("dirname", value))
        
        /// Specifies that an <input> element should be disabled
        [<Extension>]
        static member disabled(this: #input, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("disabled", null) else x.node.removeAttribute("disabled") )
        
        /// Specifies that an <input> element should be disabled
        [<Extension>]
        static member disabled(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("disabled", value))
        
        /// Specifies the form the <input> element belongs to
        [<Extension>]
        static member form(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("form", value))
        
        /// Specifies the URL of the file that will process the input control when the form is submitted (for type="submit" and type="image")
        [<Extension>]
        static member formaction(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formaction", value))
        
        /// Specifies how the form-data should be encoded when submitting it to the server (for type="submit" and type="image")
        [<Extension>]
        static member formenctype(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formenctype", value))
        
        /// Specifies how the form-data should be encoded when submitting it to the server (for type="submit" and type="image")
        [<Extension>]
        static member formenctype(this: #input, value: input.``formenctype``) =
            this.onEval(fun x -> x.node.setAttribute("formenctype", value.ToString()))
        
        /// Defines the HTTP method for sending data to the action URL (for type="submit" and type="image")
        [<Extension>]
        static member formmethod(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formmethod", value))
        
        /// Defines the HTTP method for sending data to the action URL (for type="submit" and type="image")
        [<Extension>]
        static member formmethod(this: #input, value: input.``formmethod``) =
            this.onEval(fun x -> x.node.setAttribute("formmethod", value.ToString()))
        
        /// Defines that form elements should not be validated when submitted
        [<Extension>]
        static member formnovalidate(this: #input, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("formnovalidate", null) else x.node.removeAttribute("formnovalidate") )
        
        /// Defines that form elements should not be validated when submitted
        [<Extension>]
        static member formnovalidate(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formnovalidate", value))
        
        /// Specifies where to display the response that is received after submitting the form (for type="submit" and type="image")
        [<Extension>]
        static member formtarget(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("formtarget", value))
        
        /// Specifies where to display the response that is received after submitting the form (for type="submit" and type="image")
        [<Extension>]
        static member formtarget(this: #input, value: input.``formtarget``) =
            this.onEval(fun x -> x.node.setAttribute("formtarget", value.ToString()))
        
        /// Specifies the height of an <input> element (only for type="image")
        [<Extension>]
        static member height(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("height", value))
        
        /// Refers to a <datalist> element that contains pre-defined options for an <input> element
        [<Extension>]
        static member list(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("list", value))
        
        /// Specifies the maximum value for an <input> element
        [<Extension>]
        static member max(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("max", value))
        
        /// Specifies the maximum value for an <input> element
        [<Extension>]
        static member max(this: #input, value: input.``max``) =
            this.onEval(fun x -> x.node.setAttribute("max", value.ToString()))
        
        /// Specifies the maximum number of characters allowed in an <input> element
        [<Extension>]
        static member maxlength(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("maxlength", value))
        
        /// Specifies a minimum value for an <input> element
        [<Extension>]
        static member min(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("min", value))
        
        /// Specifies a minimum value for an <input> element
        [<Extension>]
        static member min(this: #input, value: input.``min``) =
            this.onEval(fun x -> x.node.setAttribute("min", value.ToString()))
        
        /// Specifies the minimum number of characters required in an <input> element
        [<Extension>]
        static member minlength(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("minlength", value))
        
        /// Specifies that a user can enter more than one value in an <input> element
        [<Extension>]
        static member multiple(this: #input, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("multiple", null) else x.node.removeAttribute("multiple") )
        
        /// Specifies that a user can enter more than one value in an <input> element
        [<Extension>]
        static member multiple(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("multiple", value))
        
        /// Specifies the name of an <input> element
        [<Extension>]
        static member name(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies a regular expression that an <input> element's value is checked against
        [<Extension>]
        static member pattern(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("pattern", value))
        
        /// Specifies a short hint that describes the expected value of an <input> element
        [<Extension>]
        static member placeholder(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("placeholder", value))
        
        /// Specifies that an input field is read-only
        [<Extension>]
        static member readonly(this: #input, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("readonly", null) else x.node.removeAttribute("readonly") )
        
        /// Specifies that an input field is read-only
        [<Extension>]
        static member readonly(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("readonly", value))
        
        /// Specifies that an input field must be filled out before submitting the form
        [<Extension>]
        static member required(this: #input, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("required", null) else x.node.removeAttribute("required") )
        
        /// Specifies that an input field must be filled out before submitting the form
        [<Extension>]
        static member required(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("required", value))
        
        /// Specifies the width, in characters, of an <input> element
        [<Extension>]
        static member size(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("size", value))
        
        /// Specifies the URL of the image to use as a submit button (only for type="image")
        [<Extension>]
        static member src(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Specifies the interval between legal numbers in an input field
        [<Extension>]
        static member step(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("step", value))
        
        /// Specifies the interval between legal numbers in an input field
        [<Extension>]
        static member step(this: #input, value: input.``step``) =
            this.onEval(fun x -> x.node.setAttribute("step", value.ToString()))
        
        /// Specifies the type <input> element to display
        [<Extension>]
        static member type'(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        
        /// Specifies the type <input> element to display
        [<Extension>]
        static member type'(this: #input, value: input.``type``) =
            this.onEval(fun x -> x.node.setAttribute("type", value.ToString()))
        
        /// Specifies the value of an <input> element
        ///  
        [<Extension>]
        static member value(this: #input, value: string) =
            this.onEval(fun x -> x.node.value <- value)
        
        /// Specifies the width of an <input> element (only for type="image")
        [<Extension>]
        static member width(this: #input, value: string) =
            this.onEval(fun x -> x.node.setAttribute("width", value))
        

        // Events
        
    end

[<Extension>]
type insExtensions =
    class
        // Attributes
        
        /// Specifies a URL to a document that explains the reason why the text was inserted/changed
        [<Extension>]
        static member cite(this: #ins, value: string) =
            this.onEval(fun x -> x.node.setAttribute("cite", value))
        
        /// Specifies the date and time when the text was inserted/changed
        [<Extension>]
        static member datetime(this: #ins, value: string) =
            this.onEval(fun x -> x.node.setAttribute("datetime", value))
        

        // Events
        
    end

[<Extension>]
type kbdExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type labelExtensions =
    class
        // Attributes
        
        /// Specifies the id of the form element the label should be bound to
        [<Extension>]
        static member for'(this: #label, value: string) =
            this.onEval(fun x -> x.node.setAttribute("for", value))
        
        /// Specifies which form the label belongs to
        [<Extension>]
        static member form(this: #label, value: string) =
            this.onEval(fun x -> x.node.setAttribute("form", value))
        

        // Events
        
    end

[<Extension>]
type legendExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type liExtensions =
    class
        // Attributes
        
        /// Only for <ol> lists. Specifies the start value of a list item. The following list items will increment from that number
        [<Extension>]
        static member value(this: #li, value: string) =
            this.onEval(fun x -> x.node.setAttribute("value", value))
        

        // Events
        
    end

[<Extension>]
type linkExtensions =
    class
        // Attributes
        
        /// Specifies how the element handles cross-origin requests
        [<Extension>]
        static member crossorigin(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("crossorigin", value))
        
        /// Specifies the location of the linked document
        [<Extension>]
        static member href(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("href", value))
        
        /// Specifies the language of the text in the linked document
        [<Extension>]
        static member hreflang(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("hreflang", value))
        
        /// Specifies on what device the linked document will be displayed
        [<Extension>]
        static member media(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("media", value))
        
        /// Specifies on what device the linked document will be displayed
        [<Extension>]
        static member media(this: #link, value: link.``media``) =
            this.onEval(fun x -> x.node.setAttribute("media", value.ToString()))
        
        /// Specifies which referrer to use when fetching the resource
        [<Extension>]
        static member referrerpolicy(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value))
        
        /// Specifies which referrer to use when fetching the resource
        [<Extension>]
        static member referrerpolicy(this: #link, value: link.``referrerpolicy``) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value.ToString()))
        
        /// Required. Specifies the relationship between the current document and the linked document
        [<Extension>]
        static member rel(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("rel", value))
        
        /// Required. Specifies the relationship between the current document and the linked document
        [<Extension>]
        static member rel(this: #link, value: link.``rel``) =
            this.onEval(fun x -> x.node.setAttribute("rel", value.ToString()))
        
        /// Specifies the size of the linked resource. Only for rel="icon"
        [<Extension>]
        static member sizes(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("sizes", value))
        
        /// Specifies the size of the linked resource. Only for rel="icon"
        [<Extension>]
        static member sizes(this: #link, value: link.``sizes``) =
            this.onEval(fun x -> x.node.setAttribute("sizes", value.ToString()))
        
        /// Defines a preferred or an alternate stylesheet
        [<Extension>]
        static member title(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("title", value))
        
        /// Specifies the media type of the linked document
        [<Extension>]
        static member type'(this: #link, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        

        // Events
        
    end

[<Extension>]
type mainExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type mapExtensions =
    class
        // Attributes
        
        /// Required. Specifies the name of the image map
        [<Extension>]
        static member name(this: #map, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        

        // Events
        
    end

[<Extension>]
type markExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type metaExtensions =
    class
        // Attributes
        
        /// Specifies the character encoding for the HTML document 
        [<Extension>]
        static member charset(this: #meta, value: string) =
            this.onEval(fun x -> x.node.setAttribute("charset", value))
        
        /// Specifies the value associated with the http-equiv or name attribute
        [<Extension>]
        static member content(this: #meta, value: string) =
            this.onEval(fun x -> x.node.setAttribute("content", value))
        
        /// Provides an HTTP header for the information/value of the content attribute
        [<Extension>]
        static member httpEquiv(this: #meta, value: string) =
            this.onEval(fun x -> x.node.setAttribute("http-equiv", value))
        
        /// Provides an HTTP header for the information/value of the content attribute
        [<Extension>]
        static member httpEquiv(this: #meta, value: meta.``http-equiv``) =
            this.onEval(fun x -> x.node.setAttribute("http-equiv", value.ToString()))
        
        /// Specifies a name for the metadata
        [<Extension>]
        static member name(this: #meta, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies a name for the metadata
        [<Extension>]
        static member name(this: #meta, value: meta.``name``) =
            this.onEval(fun x -> x.node.setAttribute("name", value.ToString()))
        

        // Events
        
    end

[<Extension>]
type meterExtensions =
    class
        // Attributes
        
        /// Specifies which form the <meter> element belongs to
        [<Extension>]
        static member form(this: #meter, value: string) =
            this.onEval(fun x -> x.node.setAttribute("form", value))
        
        /// Specifies the range that is considered to be a high value
        [<Extension>]
        static member high(this: #meter, value: string) =
            this.onEval(fun x -> x.node.setAttribute("high", value))
        
        /// Specifies the range that is considered to be a low value
        [<Extension>]
        static member low(this: #meter, value: string) =
            this.onEval(fun x -> x.node.setAttribute("low", value))
        
        /// Specifies the maximum value of the range
        [<Extension>]
        static member max(this: #meter, value: string) =
            this.onEval(fun x -> x.node.setAttribute("max", value))
        
        /// Specifies the minimum value of the range. Default value is 0
        [<Extension>]
        static member min(this: #meter, value: string) =
            this.onEval(fun x -> x.node.setAttribute("min", value))
        
        /// Specifies what value is the optimal value for the gauge
        [<Extension>]
        static member optimum(this: #meter, value: string) =
            this.onEval(fun x -> x.node.setAttribute("optimum", value))
        
        /// Required. Specifies the current value of the gauge
        [<Extension>]
        static member value(this: #meter, value: string) =
            this.onEval(fun x -> x.node.setAttribute("value", value))
        

        // Events
        
    end

[<Extension>]
type navExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type noscriptExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type objectExtensions =
    class
        // Attributes
        
        /// Specifies the URL of the resource to be used by the object
        [<Extension>]
        static member data(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("data", value))
        
        /// Specifies which form the object belongs to
        [<Extension>]
        static member form(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("form", value))
        
        /// Specifies the height of the object
        [<Extension>]
        static member height(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("height", value))
        
        /// Specifies a name for the object
        [<Extension>]
        static member name(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies the media type of data specified in the data attribute
        [<Extension>]
        static member type'(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        
        /// Specifies whether the type attribute and the actual content of the resource must match to be displayed
        [<Extension>]
        static member typemustmatch(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("typemustmatch", value))
        
        /// Specifies the name of a client-side image map to be used with the object
        [<Extension>]
        static member usemap(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("usemap", value))
        
        /// Specifies the width of the object
        [<Extension>]
        static member width(this: #object, value: string) =
            this.onEval(fun x -> x.node.setAttribute("width", value))
        

        // Events
        
    end

[<Extension>]
type olExtensions =
    class
        // Attributes
        
        /// Specifies that the list order should be reversed (9,8,7...)
        [<Extension>]
        static member reversed(this: #ol, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("reversed", null) else x.node.removeAttribute("reversed") )
        
        /// Specifies that the list order should be reversed (9,8,7...)
        [<Extension>]
        static member reversed(this: #ol, value: string) =
            this.onEval(fun x -> x.node.setAttribute("reversed", value))
        
        /// Specifies the start value of an ordered list
        [<Extension>]
        static member start(this: #ol, value: string) =
            this.onEval(fun x -> x.node.setAttribute("start", value))
        
        /// Specifies the kind of marker to use in the list
        [<Extension>]
        static member type'(this: #ol, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        
        /// Specifies the kind of marker to use in the list
        [<Extension>]
        static member type'(this: #ol, value: ol.``type``) =
            this.onEval(fun x -> x.node.setAttribute("type", value.ToString()))
        

        // Events
        
    end

[<Extension>]
type optgroupExtensions =
    class
        // Attributes
        
        /// Specifies that an option-group should be disabled
        [<Extension>]
        static member disabled(this: #optgroup, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("disabled", null) else x.node.removeAttribute("disabled") )
        
        /// Specifies that an option-group should be disabled
        [<Extension>]
        static member disabled(this: #optgroup, value: string) =
            this.onEval(fun x -> x.node.setAttribute("disabled", value))
        
        /// Specifies a label for an option-group
        [<Extension>]
        static member label(this: #optgroup, value: string) =
            this.onEval(fun x -> x.node.setAttribute("label", value))
        

        // Events
        
    end

[<Extension>]
type pExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type paramExtensions =
    class
        // Attributes
        
        /// Specifies the name of a parameter
        [<Extension>]
        static member name(this: #param, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies the value of the parameter
        [<Extension>]
        static member value(this: #param, value: string) =
            this.onEval(fun x -> x.node.setAttribute("value", value))
        

        // Events
        
    end

[<Extension>]
type pictureExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type preExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type progressExtensions =
    class
        // Attributes
        
        /// Specifies how much work the task requires in total. Default value is 1
        [<Extension>]
        static member max(this: #progress, value: string) =
            this.onEval(fun x -> x.node.setAttribute("max", value))
        
        /// Specifies how much of the task has been completed
        [<Extension>]
        static member value(this: #progress, value: string) =
            this.onEval(fun x -> x.node.setAttribute("value", value))
        

        // Events
        
    end

[<Extension>]
type qExtensions =
    class
        // Attributes
        
        /// Specifies the source URL of the quote
        [<Extension>]
        static member cite(this: #q, value: string) =
            this.onEval(fun x -> x.node.setAttribute("cite", value))
        

        // Events
        
    end

[<Extension>]
type rpExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type rtExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type rubyExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type sExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type sampExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type scriptExtensions =
    class
        // Attributes
        
        /// Specifies that the script is downloaded in parallel to parsing the page, and executed as soon as it is available (before parsing completes) (only for external scripts)
        [<Extension>]
        static member async(this: #script, value: bool) =
            this.onEval(fun x -> x.node.setAttribute("async", if value then "true" else "false" ))
        
        /// Specifies that the script is downloaded in parallel to parsing the page, and executed as soon as it is available (before parsing completes) (only for external scripts)
        [<Extension>]
        static member async(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("async", value))
        
        /// Sets the mode of the request to an HTTP CORS Request
        [<Extension>]
        static member crossorigin(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("crossorigin", value))
        
        /// Specifies that the script is downloaded in parallel to parsing the page, and executed after the page has finished parsing (only for external scripts)
        [<Extension>]
        static member defer(this: #script, value: bool) =
            this.onEval(fun x -> x.node.setAttribute("defer", if value then "true" else "false" ))
        
        /// Specifies that the script is downloaded in parallel to parsing the page, and executed after the page has finished parsing (only for external scripts)
        [<Extension>]
        static member defer(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("defer", value))
        
        /// Allows a browser to check the fetched script to ensure that the code is never loaded if the source has been manipulated
        [<Extension>]
        static member integrity(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("integrity", value))
        
        /// Specifies that the script should not be executed in browsers supporting ES2015 modules 
        [<Extension>]
        static member nomodule(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("nomodule", value))
        
        /// Specifies which referrer information to send when fetching a script
        [<Extension>]
        static member referrerpolicy(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value))
        
        /// Specifies which referrer information to send when fetching a script
        [<Extension>]
        static member referrerpolicy(this: #script, value: script.``referrerpolicy``) =
            this.onEval(fun x -> x.node.setAttribute("referrerpolicy", value.ToString()))
        
        /// Specifies the URL of an external script file
        [<Extension>]
        static member src(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Specifies the media type of the script
        [<Extension>]
        static member type'(this: #script, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        

        // Events
        
    end

[<Extension>]
type sectionExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type slotExtensions =
    class
        // Attributes
        
        /// A string used to get and set the slot's name.
        [<Extension>]
        static member name(this: #slot, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        

        // Events
        
    end

[<Extension>]
type smallExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type sourceExtensions =
    class
        // Attributes
        
        /// Accepts any valid media query that would normally be defined in a CSS 
        [<Extension>]
        static member media(this: #source, value: string) =
            this.onEval(fun x -> x.node.setAttribute("media", value))
        
        /// Accepts any valid media query that would normally be defined in a CSS 
        [<Extension>]
        static member media(this: #source, value: source.``media``) =
            this.onEval(fun x -> x.node.setAttribute("media", value.ToString()))
        
        /// Specifies image sizes for different page layouts
        [<Extension>]
        static member sizes(this: #source, value: string) =
            this.onEval(fun x -> x.node.setAttribute("sizes", value))
        
        /// Required when <source> is used in <audio> and <video>. Specifies the URL of the media file
        [<Extension>]
        static member src(this: #source, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Required when <source> is used in <picture>. Specifies the URL of the image to use in different situations
        [<Extension>]
        static member srcset(this: #source, value: string) =
            this.onEval(fun x -> x.node.setAttribute("srcset", value))
        
        /// Specifies the MIME-type of the resource
        [<Extension>]
        static member type'(this: #source, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        

        // Events
        
    end

[<Extension>]
type spanExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type strongExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type styleExtensions =
    class
        // Attributes
        
        /// Specifies what media/device the media resource is optimized for
        [<Extension>]
        static member media(this: #style, value: string) =
            this.onEval(fun x -> x.node.setAttribute("media", value))
        
        /// Specifies what media/device the media resource is optimized for
        [<Extension>]
        static member media(this: #style, value: style.``media``) =
            this.onEval(fun x -> x.node.setAttribute("media", value.ToString()))
        
        /// Specifies the media type of the <style> tag
        [<Extension>]
        static member type'(this: #style, value: string) =
            this.onEval(fun x -> x.node.setAttribute("type", value))
        

        // Events
        
    end

[<Extension>]
type subExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type summaryExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type supExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type tableExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type tbodyExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type tdExtensions =
    class
        // Attributes
        
        /// Specifies the number of columns a cell should span
        [<Extension>]
        static member colspan(this: #td, value: string) =
            this.onEval(fun x -> x.node.setAttribute("colspan", value))
        
        /// Specifies one or more header cells a cell is related to
        [<Extension>]
        static member headers(this: #td, value: string) =
            this.onEval(fun x -> x.node.setAttribute("headers", value))
        
        /// Sets the number of rows a cell should span
        [<Extension>]
        static member rowspan(this: #td, value: string) =
            this.onEval(fun x -> x.node.setAttribute("rowspan", value))
        

        // Events
        
    end

[<Extension>]
type templateExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type textareaExtensions =
    class
        // Attributes
        
        /// Specifies that a text area should automatically get focus when the page loads
        [<Extension>]
        static member autofocus(this: #textarea, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("autofocus", null) else x.node.removeAttribute("autofocus") )
        
        /// Specifies that a text area should automatically get focus when the page loads
        [<Extension>]
        static member autofocus(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("autofocus", value))
        
        /// Specifies the visible width of a text area
        [<Extension>]
        static member cols(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("cols", value))
        
        /// Specifies that the text direction of the textarea will be submitted
        [<Extension>]
        static member dirname(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("dirname", value))
        
        /// Specifies that a text area should be disabled
        [<Extension>]
        static member disabled(this: #textarea, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("disabled", null) else x.node.removeAttribute("disabled") )
        
        /// Specifies that a text area should be disabled
        [<Extension>]
        static member disabled(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("disabled", value))
        
        /// Specifies which form the text area belongs to
        [<Extension>]
        static member form(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("form", value))
        
        /// Specifies the maximum number of characters allowed in the text area
        [<Extension>]
        static member maxlength(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("maxlength", value))
        
        /// Specifies a name for a text area
        [<Extension>]
        static member name(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("name", value))
        
        /// Specifies a short hint that describes the expected value of a text area
        [<Extension>]
        static member placeholder(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("placeholder", value))
        
        /// Specifies that a text area should be read-only
        [<Extension>]
        static member readonly(this: #textarea, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("readonly", null) else x.node.removeAttribute("readonly") )
        
        /// Specifies that a text area should be read-only
        [<Extension>]
        static member readonly(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("readonly", value))
        
        /// Specifies that a text area is required/must be filled out
        [<Extension>]
        static member required(this: #textarea, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("required", null) else x.node.removeAttribute("required") )
        
        /// Specifies that a text area is required/must be filled out
        [<Extension>]
        static member required(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("required", value))
        
        /// Specifies the visible number of lines in a text area
        [<Extension>]
        static member rows(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("rows", value))
        
        /// Specifies how the text in a text area is to be wrapped when submitted in a form
        [<Extension>]
        static member wrap(this: #textarea, value: string) =
            this.onEval(fun x -> x.node.setAttribute("wrap", value))
        
        /// Specifies how the text in a text area is to be wrapped when submitted in a form
        [<Extension>]
        static member wrap(this: #textarea, value: textarea.``wrap``) =
            this.onEval(fun x -> x.node.setAttribute("wrap", value.ToString()))
        

        // Events
        
    end

[<Extension>]
type tfootExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type thExtensions =
    class
        // Attributes
        
        /// Specifies an abbreviated version of the content in a header cell
        [<Extension>]
        static member abbr(this: #th, value: string) =
            this.onEval(fun x -> x.node.setAttribute("abbr", value))
        
        /// Specifies the number of columns a header cell should span
        [<Extension>]
        static member colspan(this: #th, value: string) =
            this.onEval(fun x -> x.node.setAttribute("colspan", value))
        
        /// Specifies one or more header cells a cell is related to
        [<Extension>]
        static member headers(this: #th, value: string) =
            this.onEval(fun x -> x.node.setAttribute("headers", value))
        
        /// Specifies the number of rows a header cell should span
        [<Extension>]
        static member rowspan(this: #th, value: string) =
            this.onEval(fun x -> x.node.setAttribute("rowspan", value))
        
        /// Specifies whether a header cell is a header for a column, row, or group of columns or rows 
        [<Extension>]
        static member scope(this: #th, value: string) =
            this.onEval(fun x -> x.node.setAttribute("scope", value))
        
        /// Specifies whether a header cell is a header for a column, row, or group of columns or rows 
        [<Extension>]
        static member scope(this: #th, value: th.``scope``) =
            this.onEval(fun x -> x.node.setAttribute("scope", value.ToString()))
        

        // Events
        
    end

[<Extension>]
type theadExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type timeExtensions =
    class
        // Attributes
        
        /// Represent a machine-readable format of the <time> element
        [<Extension>]
        static member datetime(this: #time, value: string) =
            this.onEval(fun x -> x.node.setAttribute("datetime", value))
        

        // Events
        
    end

[<Extension>]
type titleExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type trExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type trackExtensions =
    class
        // Attributes
        
        /// Specifies that the track is to be enabled if the user's preferences do not indicate that another track would be more appropriate
        [<Extension>]
        static member default'(this: #track, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("default", null) else x.node.removeAttribute("default") )
        
        /// Specifies that the track is to be enabled if the user's preferences do not indicate that another track would be more appropriate
        [<Extension>]
        static member default'(this: #track, value: string) =
            this.onEval(fun x -> x.node.setAttribute("default", value))
        
        /// Specifies the kind of text track
        [<Extension>]
        static member kind(this: #track, value: string) =
            this.onEval(fun x -> x.node.setAttribute("kind", value))
        
        /// Specifies the kind of text track
        [<Extension>]
        static member kind(this: #track, value: track.``kind``) =
            this.onEval(fun x -> x.node.setAttribute("kind", value.ToString()))
        
        /// Specifies the title of the text track
        [<Extension>]
        static member label(this: #track, value: string) =
            this.onEval(fun x -> x.node.setAttribute("label", value))
        
        /// Required. Specifies the URL of the track file
        [<Extension>]
        static member src(this: #track, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Specifies the language of the track text data (required if kind="subtitles")
        [<Extension>]
        static member srclang(this: #track, value: string) =
            this.onEval(fun x -> x.node.setAttribute("srclang", value))
        

        // Events
        
    end

[<Extension>]
type uExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type ulExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type varExtensions =
    class
        // Attributes
        

        // Events
        
    end

[<Extension>]
type videoExtensions =
    class
        // Attributes
        
        /// Specifies that the video will start playing as soon as it is ready
        [<Extension>]
        static member autoplay(this: #video, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("autoplay", null) else x.node.removeAttribute("autoplay") )
        
        /// Specifies that the video will start playing as soon as it is ready
        [<Extension>]
        static member autoplay(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("autoplay", value))
        
        /// Specifies that video controls should be displayed (such as a play/pause button etc).
        [<Extension>]
        static member controls(this: #video, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("controls", null) else x.node.removeAttribute("controls") )
        
        /// Specifies that video controls should be displayed (such as a play/pause button etc).
        [<Extension>]
        static member controls(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("controls", value))
        
        /// Sets the height of the video player
        [<Extension>]
        static member height(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("height", value))
        
        /// Specifies that the video will start over again, every time it is finished
        [<Extension>]
        static member loop(this: #video, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("loop", null) else x.node.removeAttribute("loop") )
        
        /// Specifies that the video will start over again, every time it is finished
        [<Extension>]
        static member loop(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("loop", value))
        
        /// Specifies that the audio output of the video should be muted
        [<Extension>]
        static member muted(this: #video, value: bool) =
            this.onEval(fun x -> if value then x.node.setAttribute("muted", null) else x.node.removeAttribute("muted") )
        
        /// Specifies that the audio output of the video should be muted
        [<Extension>]
        static member muted(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("muted", value))
        
        /// Specifies an image to be shown while the video is downloading, or until the user hits the play button
        [<Extension>]
        static member poster(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("poster", value))
        
        /// Specifies if and how the author thinks the video should be loaded when the page loads
        [<Extension>]
        static member preload(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("preload", value))
        
        /// Specifies if and how the author thinks the video should be loaded when the page loads
        [<Extension>]
        static member preload(this: #video, value: video.``preload``) =
            this.onEval(fun x -> x.node.setAttribute("preload", value.ToString()))
        
        /// Specifies the URL of the video file
        [<Extension>]
        static member src(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("src", value))
        
        /// Sets the width of the video player
        [<Extension>]
        static member width(this: #video, value: string) =
            this.onEval(fun x -> x.node.setAttribute("width", value))
        

        // Events
        
    end

[<Extension>]
type wbrExtensions =
    class
        // Attributes
        

        // Events
        
    end


type Html =
    
    /// Defines a hyperlink
    static member inline a = HtmlElementBuilders.a()
    
    /// Defines an abbreviation or an acronym
    static member inline abbr = HtmlElementBuilders.abbr()
    
    /// Defines contact information for the author/owner of a document
    static member inline address = HtmlElementBuilders.address()
    
    /// Defines an area inside an image map
    static member inline area = HtmlElementBuilders.area()
    
    /// Defines an article
    static member inline article = HtmlElementBuilders.article()
    
    /// Defines content aside from the page content
    static member inline aside = HtmlElementBuilders.aside()
    
    /// Defines embedded sound content
    static member inline audio = HtmlElementBuilders.audio()
    
    /// Defines bold text
    static member inline b = HtmlElementBuilders.b()
    
    /// Specifies the base URL/target for all relative URLs in a document
    static member inline base' = HtmlElementBuilders.base'()
    
    /// Isolates a part of text that might be formatted in a different direction from other text outside it
    static member inline bdi = HtmlElementBuilders.bdi()
    
    /// Overrides the current text direction
    static member inline bdo = HtmlElementBuilders.bdo()
    
    /// Defines a section that is quoted from another source
    static member inline blockquote = HtmlElementBuilders.blockquote()
    
    /// Defines the document's body
    static member inline body = HtmlElementBuilders.body()
    
    /// Defines a single line break
    static member inline br = HtmlElementBuilders.br()
    
    /// Defines a clickable button
    static member inline button = HtmlElementBuilders.button()
    
    /// Used to draw graphics, on the fly, via scripting (usually JavaScript)
    static member inline canvas = HtmlElementBuilders.canvas()
    
    /// Defines a table caption
    static member inline caption = HtmlElementBuilders.caption()
    
    /// Defines the title of a work
    static member inline cite = HtmlElementBuilders.cite()
    
    /// Defines a piece of computer code
    static member inline code = HtmlElementBuilders.code()
    
    /// Specifies column properties for each column within a <colgroup> element 
    static member inline col = HtmlElementBuilders.col()
    
    /// Specifies a group of one or more columns in a table for formatting
    static member inline colgroup = HtmlElementBuilders.colgroup()
    
    /// Adds a machine-readable translation of a given content
    static member inline data = HtmlElementBuilders.data()
    
    /// Defines a description/value of a term in a description list
    static member inline dd = HtmlElementBuilders.dd()
    
    /// Defines text that has been deleted from a document
    static member inline del = HtmlElementBuilders.del()
    
    /// Defines additional details that the user can view or hide
    static member inline details = HtmlElementBuilders.details()
    
    /// Specifies a term that is going to be defined within the content
    static member inline dfn = HtmlElementBuilders.dfn()
    
    /// Defines a dialog box or window
    static member inline dialog = HtmlElementBuilders.dialog()
    
    /// Defines a section in a document
    static member inline div = HtmlElementBuilders.div()
    
    /// Defines a description list
    static member inline dl = HtmlElementBuilders.dl()
    
    /// Defines a term/name in a description list
    static member inline dt = HtmlElementBuilders.dt()
    
    /// Defines emphasized text 
    static member inline em = HtmlElementBuilders.em()
    
    /// Defines a container for an external application
    static member inline embed = HtmlElementBuilders.embed()
    
    /// Groups related elements in a form
    static member inline fieldset = HtmlElementBuilders.fieldset()
    
    /// Defines a caption for a <figure> element
    static member inline figcaption = HtmlElementBuilders.figcaption()
    
    /// Specifies self-contained content
    static member inline figure = HtmlElementBuilders.figure()
    
    /// Defines a footer for a document or section
    static member inline footer = HtmlElementBuilders.footer()
    
    /// Defines an HTML form for user input
    static member inline form = HtmlElementBuilders.form()
    
    /// Defines HTML headings
    static member inline h1 = HtmlElementBuilders.h1()
    
    /// Defines HTML headings
    static member inline h2 = HtmlElementBuilders.h2()
    
    /// Defines HTML headings
    static member inline h3 = HtmlElementBuilders.h3()
    
    /// Defines HTML headings
    static member inline h4 = HtmlElementBuilders.h4()
    
    /// Defines HTML headings
    static member inline h5 = HtmlElementBuilders.h5()
    
    /// Defines HTML headings
    static member inline h6 = HtmlElementBuilders.h6()
    
    /// Contains metadata/information for the document
    static member inline head = HtmlElementBuilders.head()
    
    /// Defines a header for a document or section
    static member inline header = HtmlElementBuilders.header()
    
    ///  Defines a thematic change in the content
    static member inline hr = HtmlElementBuilders.hr()
    
    /// Defines the root of an HTML document
    static member inline html = HtmlElementBuilders.html()
    
    /// Defines a part of text in an alternate voice or mood
    static member inline i = HtmlElementBuilders.i()
    
    /// Defines an inline frame
    static member inline iframe = HtmlElementBuilders.iframe()
    
    /// Defines an image
    static member inline img = HtmlElementBuilders.img()
    
    /// Defines an input control
    static member inline input = HtmlElementBuilders.input()
    
    /// Defines a text that has been inserted into a document
    static member inline ins = HtmlElementBuilders.ins()
    
    /// Defines keyboard input
    static member inline kbd = HtmlElementBuilders.kbd()
    
    /// Defines a label for an <input> element
    static member inline label = HtmlElementBuilders.label()
    
    /// Defines a caption for a <fieldset> element
    static member inline legend = HtmlElementBuilders.legend()
    
    /// Defines a list item
    static member inline li = HtmlElementBuilders.li()
    
    /// Defines the relationship between a document and an external resource (most used to link to style sheets)
    static member inline link = HtmlElementBuilders.link()
    
    /// Specifies the main content of a document
    static member inline main = HtmlElementBuilders.main()
    
    /// Defines an image map
    static member inline map = HtmlElementBuilders.map()
    
    /// Defines marked/highlighted text
    static member inline mark = HtmlElementBuilders.mark()
    
    /// Defines metadata about an HTML document
    static member inline meta = HtmlElementBuilders.meta()
    
    /// Defines a scalar measurement within a known range (a gauge)
    static member inline meter = HtmlElementBuilders.meter()
    
    /// Defines navigation links
    static member inline nav = HtmlElementBuilders.nav()
    
    /// Defines an alternate content for users that do not support client-side scripts
    static member inline noscript = HtmlElementBuilders.noscript()
    
    /// Defines a container for an external application
    static member inline object = HtmlElementBuilders.object()
    
    /// Defines an ordered list
    static member inline ol = HtmlElementBuilders.ol()
    
    /// Defines a group of related options in a drop-down list
    static member inline optgroup = HtmlElementBuilders.optgroup()
    
    /// Defines a paragraph
    static member inline p = HtmlElementBuilders.p()
    
    /// Defines a parameter for an object
    static member inline param = HtmlElementBuilders.param()
    
    /// Defines a container for multiple image resources
    static member inline picture = HtmlElementBuilders.picture()
    
    /// Defines preformatted text
    static member inline pre = HtmlElementBuilders.pre()
    
    /// Represents the progress of a task
    static member inline progress = HtmlElementBuilders.progress()
    
    /// Defines a short quotation
    static member inline q = HtmlElementBuilders.q()
    
    /// Defines what to show in browsers that do not support ruby annotations
    static member inline rp = HtmlElementBuilders.rp()
    
    /// Defines an explanation/pronunciation of characters (for East Asian typography)
    static member inline rt = HtmlElementBuilders.rt()
    
    /// Defines a ruby annotation (for East Asian typography)
    static member inline ruby = HtmlElementBuilders.ruby()
    
    /// Defines text that is no longer correct
    static member inline s = HtmlElementBuilders.s()
    
    /// Defines sample output from a computer program
    static member inline samp = HtmlElementBuilders.samp()
    
    /// Defines a client-side script
    static member inline script = HtmlElementBuilders.script()
    
    /// Defines a section in a document
    static member inline section = HtmlElementBuilders.section()
    
    /// The slot element�part of the Web Components technology suite is a placeholder inside a web component that you can fill with your own markup, which lets you create separate DOM trees and present them together.
    static member inline slot = HtmlElementBuilders.slot()
    
    /// Defines smaller text
    static member inline small = HtmlElementBuilders.small()
    
    /// Defines multiple media resources for media elements (<video> and <audio>)
    static member inline source = HtmlElementBuilders.source()
    
    /// Defines a section in a document
    static member inline span = HtmlElementBuilders.span()
    
    /// Defines important text
    static member inline strong = HtmlElementBuilders.strong()
    
    /// Defines style information for a document
    static member inline style = HtmlElementBuilders.style()
    
    /// Defines subscripted text
    static member inline sub = HtmlElementBuilders.sub()
    
    /// Defines a visible heading for a <details> element
    static member inline summary = HtmlElementBuilders.summary()
    
    /// Defines superscripted text
    static member inline sup = HtmlElementBuilders.sup()
    
    /// Defines a table
    static member inline table = HtmlElementBuilders.table()
    
    /// Groups the body content in a table
    static member inline tbody = HtmlElementBuilders.tbody()
    
    /// Defines a cell in a table
    static member inline td = HtmlElementBuilders.td()
    
    /// Defines a container for content that should be hidden when the page loads
    static member inline template = HtmlElementBuilders.template()
    
    /// Defines a multiline input control (text area)
    static member inline textarea = HtmlElementBuilders.textarea()
    
    /// Groups the footer content in a table
    static member inline tfoot = HtmlElementBuilders.tfoot()
    
    /// Defines a header cell in a table
    static member inline th = HtmlElementBuilders.th()
    
    /// Groups the header content in a table
    static member inline thead = HtmlElementBuilders.thead()
    
    /// Defines a specific time (or datetime)
    static member inline time = HtmlElementBuilders.time()
    
    /// Defines a title for the document
    static member inline title = HtmlElementBuilders.title()
    
    /// Defines a row in a table
    static member inline tr = HtmlElementBuilders.tr()
    
    /// Defines text tracks for media elements (<video> and <audio>)
    static member inline track = HtmlElementBuilders.track()
    
    /// Defines some text that is unarticulated and styled differently from normal text
    static member inline u = HtmlElementBuilders.u()
    
    /// Defines an unordered list
    static member inline ul = HtmlElementBuilders.ul()
    
    /// Defines a variable
    static member inline var = HtmlElementBuilders.var()
    
    /// Defines embedded video content
    static member inline video = HtmlElementBuilders.video()
    
    /// Defines a possible line-break
    static member inline wbr = HtmlElementBuilders.wbr()
    
