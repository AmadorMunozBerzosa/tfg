||| Defines the valid attributes in HTML and the different formats they can have
module Core.Attributes

import Data.String


||| HTML attribute that is valid for some HTML element
public export
data Attribute =
      Accesskey
    | Abbr
    | Accept
    | AcceptCharset
    | Action
    | Allow
    | Allowfullscreen
    | Alpha
    | Alt
    | As
    | Async
    | Autocapitalize
    | Autocomplete
    | Autocorrect
    | Autofocus
    | Autoplay
    | Blocking
    | Charset
    | Checked
    | Cite
    | Class
    | ClosedBy
    | Color
    | Colorspace
    | Cols
    | Colspan
    | Command
    | Commandfor
    | Content
    | Contenteditable
    | Controls
    | Coords
    | Crossorigin
    | Data
    | Datetime
    | Decoding
    | Default
    | Defer
    | Dir
    | Dirname
    | Disabled
    | Download
    | Draggable
    | Enctype
    | Enterkeyhint
    | Fetchpriority
    | For
    | Form
    | Formaction
    | Formenctype
    | Formmethod
    | Formnovalidate
    | Formtarget
    | Headers
    | Headingoffset
    | Headingreset
    | Height
    | Hidden
    | High
    | Href
    | Hreflang
    | HttpEquiv
    | Id
    | Imagesizes
    | Imagesrcset
    | Inert
    | Inputmode
    | Integrity
    | Is
    | Ismap
    | Itemid
    | Itemprop
    | Itemref
    | Itemscope
    | Itemtype
    | Kind
    | Label
    | Lang
    | List
    | Loading
    | Loop
    | Low
    | Max
    | Maxlength
    | Media
    | Method
    | Min
    | Minlength
    | Multiple
    | Muted
    | Name
    | Nonce
    | Nomodule
    | Novalidate
    | Open
    | Optimum
    | Pattern
    | Ping
    | Placeholder
    | Playsinline
    | Popover
    | Popovertarget
    | Popovertargetaction
    | Poster
    | Preload
    | Readonly
    | Referrerpolicy
    | Rel
    | Required
    | Reversed
    | Rows
    | Rowspan
    | Sandbox
    | Scope
    | Selected
    | Shadowrootmode
    | Shadowrootdelegatesfocus
    | Shadowrootslotassignment
    | Shadowrootclonable
    | Shadowrootserializable
    | Shadowrootcustomelementregistry
    | Shape
    | Size
    | Sizes
    | Slot
    | Span
    | Spellcheck
    | Src
    | Srcdoc
    | Srclang
    | Srcset
    | Start
    | Step
    | Style
    | Tabindex
    | Target
    | Title
    | Translate
    | Type'
    | Usemap
    | Value
    | Width
    | Wrap
    | Writingsuggestions
    -- Global events
    | Onauxclick
    | Onbeforeinput
    | Onbeforematch
    | Onbeforetoggle
    | Onblur
    | Oncancel
    | Oncanplay
    | Oncanplaythrough
    | Onchange
    | Onclick
    | Onclose
    | Oncommand
    | Oncontextlost
    | Oncontextmenu
    | Oncontextrestored
    | Oncopy
    | Oncuechange
    | Oncut
    | Ondblclick
    | Ondrag
    | Ondragend
    | Ondragenter
    | Ondragleave
    | Ondragover
    | Ondragstart
    | Ondrop
    | Ondurationchange
    | Onemptied
    | Onended
    | Onerror
    | Onfocus
    | Onformdata
    | Oninput
    | Oninvalid
    | Onkeydown
    | Onkeypress
    | Onkeyup
    | Onload
    | Onloadeddata
    | Onloadedmetadata
    | Onloadstart
    | Onmousedown
    | Onmouseenter
    | Onmouseleave
    | Onmousemove
    | Onmouseout
    | Onmouseover
    | Onmouseup
    | Onpaste
    | Onpause
    | Onplay
    | Onplaying
    | Onprogress
    | Onratechange
    | Onreset
    | Onresize
    | Onscroll
    | Onscrollend
    | Onsecuritypolicyviolation
    | Onseeked
    | Onseeking
    | Onselect
    | Onslotchange
    | Onstalled
    | Onsubmit
    | Onsuspend
    | Ontimeupdate
    | Ontoggle
    | Onvolumechange
    | Onwaiting
    | Onwheel
    -- Other events
    | Onafterprint
    | Onbeforeprint
    | Onbeforeunload
    | Onhashchange
    | Onlanguagechange
    | Onmessage
    | Onmessageerror
    | Onoffline
    | Ononline
    | Onpagehide
    | Onpagereveal
    | Onpageshow
    | Onpageswap
    | Onpopstate
    | Onrejectionhandled
    | Onstorage
    | Onunhandledrejection
    | Onunload
    | Custom String

public export
Eq Attribute where
    Accesskey == Accesskey = True
    Abbr == Abbr = True
    Accept == Accept = True
    AcceptCharset == AcceptCharset = True
    Action == Action = True
    Allow == Allow = True
    Allowfullscreen == Allowfullscreen = True
    Alpha == Alpha = True
    Alt == Alt = True
    As == As = True
    Async == Async = True
    Autocapitalize == Autocapitalize = True
    Autocomplete == Autocomplete = True
    Autocorrect == Autocorrect = True
    Autofocus == Autofocus = True
    Autoplay == Autoplay = True
    Blocking == Blocking = True
    Charset == Charset = True
    Checked == Checked = True
    Cite == Cite = True
    Class == Class = True
    ClosedBy == ClosedBy = True
    Color == Color = True
    Colorspace == Colorspace = True
    Cols == Cols = True
    Colspan == Colspan = True
    Command == Command = True
    Commandfor == Commandfor = True
    Content == Content = True
    Contenteditable == Contenteditable = True
    Controls == Controls = True
    Coords == Coords = True
    Crossorigin == Crossorigin = True
    Data == Data = True
    Datetime == Datetime = True
    Decoding == Decoding = True
    Default == Default = True
    Defer == Defer = True
    Dir == Dir = True
    Dirname == Dirname = True
    Disabled == Disabled = True
    Download == Download = True
    Draggable == Draggable = True
    Enctype == Enctype = True
    Enterkeyhint == Enterkeyhint = True
    Fetchpriority == Fetchpriority = True
    For == For = True
    Form == Form = True
    Formaction == Formaction = True
    Formenctype == Formenctype = True
    Formmethod == Formmethod = True
    Formnovalidate == Formnovalidate = True
    Formtarget == Formtarget = True
    Headers == Headers = True
    Headingoffset == Headingoffset = True
    Headingreset == Headingreset = True
    Height == Height = True
    Hidden == Hidden = True
    High == High = True
    Href == Href = True
    Hreflang == Hreflang = True
    HttpEquiv == HttpEquiv = True
    Id == Id = True
    Imagesizes == Imagesizes = True
    Imagesrcset == Imagesrcset = True
    Inert == Inert = True
    Inputmode == Inputmode = True
    Integrity == Integrity = True
    Is == Is = True
    Ismap == Ismap = True
    Itemid == Itemid = True
    Itemprop == Itemprop = True
    Itemref == Itemref = True
    Itemscope == Itemscope = True
    Itemtype == Itemtype = True
    Kind == Kind = True
    Label == Label = True
    Lang == Lang = True
    List == List = True
    Loading == Loading = True
    Loop == Loop = True
    Low == Low = True
    Max == Max = True
    Maxlength == Maxlength = True
    Media == Media = True
    Method == Method = True
    Min == Min = True
    Minlength == Minlength = True
    Multiple == Multiple = True
    Muted == Muted = True
    Name == Name = True
    Nonce == Nonce = True
    Nomodule == Nomodule = True
    Novalidate == Novalidate = True
    Open == Open = True
    Optimum == Optimum = True
    Pattern == Pattern = True
    Ping == Ping = True
    Placeholder == Placeholder = True
    Playsinline == Playsinline = True
    Popover == Popover = True
    Popovertarget == Popovertarget = True
    Popovertargetaction == Popovertargetaction = True
    Poster == Poster = True
    Preload == Preload = True
    Readonly == Readonly = True
    Referrerpolicy == Referrerpolicy = True
    Rel == Rel = True
    Required == Required = True
    Reversed == Reversed = True
    Rows == Rows = True
    Rowspan == Rowspan = True
    Sandbox == Sandbox = True
    Scope == Scope = True
    Selected == Selected = True
    Shadowrootmode == Shadowrootmode = True
    Shadowrootdelegatesfocus == Shadowrootdelegatesfocus = True
    Shadowrootslotassignment == Shadowrootslotassignment = True
    Shadowrootclonable == Shadowrootclonable = True
    Shadowrootserializable == Shadowrootserializable = True
    Shadowrootcustomelementregistry == Shadowrootcustomelementregistry = True
    Shape == Shape = True
    Size == Size = True
    Sizes == Sizes = True
    Slot == Slot = True
    Span == Span = True
    Spellcheck == Spellcheck = True
    Src == Src = True
    Srcdoc == Srcdoc = True
    Srclang == Srclang = True
    Srcset == Srcset = True
    Start == Start = True
    Step == Step = True
    Style == Style = True
    Tabindex == Tabindex = True
    Target == Target = True
    Title == Title = True
    Translate == Translate = True
    Type' == Type' =True
    Usemap == Usemap = True
    Value == Value = True
    Width == Width = True
    Wrap == Wrap = True
    Writingsuggestions == Writingsuggestions = True
    Onauxclick == Onauxclick = True
    Onbeforeinput == Onbeforeinput = True
    Onbeforematch == Onbeforematch = True
    Onbeforetoggle == Onbeforetoggle = True
    Onblur == Onblur = True
    Oncancel == Oncancel = True
    Oncanplay == Oncanplay = True
    Oncanplaythrough == Oncanplaythrough = True
    Onchange == Onchange = True
    Onclick == Onclick = True
    Onclose == Onclose = True
    Oncommand == Oncommand = True
    Oncontextlost == Oncontextlost = True
    Oncontextmenu == Oncontextmenu = True
    Oncontextrestored == Oncontextrestored = True
    Oncopy == Oncopy = True
    Oncuechange == Oncuechange = True
    Oncut == Oncut = True
    Ondblclick == Ondblclick = True
    Ondrag == Ondrag = True
    Ondragend == Ondragend = True
    Ondragenter == Ondragenter = True
    Ondragleave == Ondragleave = True
    Ondragover == Ondragover = True
    Ondragstart == Ondragstart = True
    Ondrop == Ondrop = True
    Ondurationchange == Ondurationchange = True
    Onemptied == Onemptied = True
    Onended == Onended = True
    Onerror == Onerror = True
    Onfocus == Onfocus = True
    Onformdata == Onformdata = True
    Oninput == Oninput = True
    Oninvalid == Oninvalid = True
    Onkeydown == Onkeydown = True
    Onkeypress == Onkeypress = True
    Onkeyup == Onkeyup = True
    Onload == Onload = True
    Onloadeddata == Onloadeddata = True
    Onloadedmetadata == Onloadedmetadata = True
    Onloadstart == Onloadstart = True
    Onmousedown == Onmousedown = True
    Onmouseenter == Onmouseenter = True
    Onmouseleave == Onmouseleave = True
    Onmousemove == Onmousemove = True
    Onmouseout == Onmouseout = True
    Onmouseover == Onmouseover = True
    Onmouseup == Onmouseup = True
    Onpaste == Onpaste = True
    Onpause == Onpause = True
    Onplay == Onplay = True
    Onplaying == Onplaying = True
    Onprogress == Onprogress = True
    Onratechange == Onratechange = True
    Onreset == Onreset = True
    Onresize == Onresize = True
    Onscroll == Onscroll = True
    Onscrollend == Onscrollend = True
    Onsecuritypolicyviolation == Onsecuritypolicyviolation = True
    Onseeked == Onseeked = True
    Onseeking == Onseeking = True
    Onselect == Onselect = True
    Onslotchange == Onslotchange = True
    Onstalled == Onstalled = True
    Onsubmit == Onsubmit = True
    Onsuspend == Onsuspend = True
    Ontimeupdate == Ontimeupdate = True
    Ontoggle == Ontoggle = True
    Onvolumechange == Onvolumechange = True
    Onwaiting == Onwaiting = True
    Onwheel == Onwheel = True
    Onafterprint == Onafterprint = True
    Onbeforeprint == Onbeforeprint = True
    Onbeforeunload == Onbeforeunload = True
    Onhashchange == Onhashchange = True
    Onlanguagechange == Onlanguagechange = True
    Onmessage == Onmessage = True
    Onmessageerror == Onmessageerror = True
    Onoffline == Onoffline = True
    Ononline == Ononline = True
    Onpagehide == Onpagehide = True
    Onpagereveal == Onpagereveal = True
    Onpageshow == Onpageshow = True
    Onpageswap == Onpageswap = True
    Onpopstate == Onpopstate = True
    Onrejectionhandled == Onrejectionhandled = True
    Onstorage == Onstorage = True
    Onunhandledrejection == Onunhandledrejection = True
    Onunload == Onunload = True
    (Custom a) == (Custom b) = a == b
    _ == _ = False



namespace Format
    ||| Required string format for a HTML attribute's value. All comparisons are case-insensitive
    public export
    data Format =
          Anything -- All values are valid
        | Text String -- An exact string
        | Trimmed String -- An exact string, optionally with surrounding whitespace
        | WhitespaceBetween String String -- The concatenation of two strings, optionally with whitespace in between
        | Prefix String -- A string that begins with a prefix
        | SingleLine -- A string without line breaks
        | NoWhitespace -- A string that doesn't contain any whitespace
        
        -- Numeric types
        | Integer' -- An integer literal
        | NonNegative -- An integer literal greater or equal to 0
        | Positive -- An integer literal greater than 0
        | Float -- A floating-point number literal
        | LessThan Double -- A floating-point number literal with a non-strict lower bound
        | GreaterThan Double -- A floating-point number literal with a non-strict upper bound

        -- Data types
        | Character -- A single character
        | List String Format -- A list of values that follow the given format, separated by the given string
        | Set String Format -- A list of values that follow the given format, separated by the given string, with no duplicates
        | Boolean -- Either the empty string, or the name of the attribute
        | Date -- A valid date string
        | Month -- A valid month string
        | Week -- A valid week string
        | Time -- A valid time string
        | Datetime -- A valid datetime string
        | Size -- A pair of naturals, in the format "nxm"

        -- Area coordinates
        | CircleCoords -- Coordinates for a circle, given by a center position and radius
        | RectCoords -- Coordinates for a rectangle, given by the position of its upper-left and lower-right vertices
        | PolygonCoords -- Coordinates for a polygon, given as a list of vertices

        -- Element references
        | NavigableTargetName -- https://html.spec.whatwg.org/multipage/document-sequences.html#navigable-target-names

        -- Web-specific
        | URL -- A valid URL string
        | NotURL -- Anything that's not a valid URL string
        | AbsoluteURL -- A non-relative URL string
        | LanguageCode -- A BCP-47 language string
        | MimeType -- A MIME Type that conforms to HTTP RFC2046
        | SrcDoc -- A string containing an HTML document
        | EventHandler -- A JS event handler
        | Color -- A CSS color string
        | ImageCandidate
        | Email -- An email address
        | FileAccept
        | Pattern -- A regex pattern
        | Autocomplete -- An autocomplete guideline
        | Integrity -- Integrity metadata

        -- External specifications
        | MediaQuery
        | PermissionsPolicy
        | ContentSecurityPolicy
        | RefreshPolicy

        -- Combinators
        | (||) Format Format -- Either one of two formats

    public export
    FromString Format where
        fromString = Text

    -- The following formats are formats that are reused within the HTML Spec
    -- We define them here to avoid duplication later

    public export
    PreloadDestination : Format
    PreloadDestination = "fetch" || "font" || "image" || "script" || "style" || "track"

    public export
    ModulePreloadDestination : Format
    ModulePreloadDestination = "json" || "style" || "text" || "audioworklet" || "paintworklet" || "script" || "serviceworker" || "sharedworker" || "worker"

    public export
    ResourceKind : Format
    ResourceKind = "fetch" || "font" || "image" || "script" || "style" || "track" || "json" || "style" || "text" || "audioworklet" || "paintworklet" || "script" || "serviceworker" || "sharedworker" || "worker"

    public export
    FetchPriority : Format
    FetchPriority = "high" || "low" || "auto"

    public export
    ReferrerPolicy : Format
    ReferrerPolicy = 
        "no-referrer-when-downgrade" ||
        "no-referrer" ||
        "same-origin" ||
        "origin-when-cross-origin" ||
        "origin" ||
        "strict-origin-when-cross-origin" ||
        "strict-origin" ||
        "unsafe-url" ||
        ""

    public export
    BlockingFormat: Format
    BlockingFormat = "render"

    public export
    Loading: Format
    Loading = "eager" || "lazy"

    public export
    Cors : Format
    Cors = "anonymous" || "use-credentials"

    public export
    Preload : Format
    Preload = "auto" || "none" || "metadata"

    public export
    NavigationKeyword : Format
    NavigationKeyword = "_blank" || "_self" || "_parent" || "_top"

    public export
    JSMimeType : Format
    JSMimeType = 
            "application/ecmascript"
            || "application/javascript"
            || "application/x-ecmascript"
            || "application/x-javascript"
            || "text/ecmascript"
            || "text/javascript"
            || "text/javascript1.0"
            || "text/javascript1.1"
            || "text/javascript1.2"
            || "text/javascript1.3"
            || "text/javascript1.4"
            || "text/javascript1.5"
            || "text/jscript"
            || "text/livescript"
            || "text/x-ecmascript"
            || "text/x-javascript"


public export
Show Attribute where
    show Accesskey = "accesskey"
    show Abbr = "abbr"
    show Accept = "accept"
    show AcceptCharset = "accept-charset"
    show Action = "action"
    show Allow = "allow"
    show Allowfullscreen = "allowfullscreen"
    show Alpha = "alpha"
    show Alt = "alt"
    show As = "as"
    show Async = "async"
    show Autocapitalize = "autocapitalize"
    show Autocomplete = "autocomplete"
    show Autocorrect = "autocorrect"
    show Autofocus = "autofocus"
    show Autoplay = "autoplay"
    show Blocking = "blocking"
    show Charset = "charset"
    show Checked = "checked"
    show Cite = "cite"
    show Class = "class"
    show ClosedBy = "closedBy"
    show Color = "color"
    show Colorspace = "colorspace"
    show Cols = "cols"
    show Colspan = "colspan"
    show Command = "command"
    show Commandfor = "commandfor"
    show Content = "content"
    show Contenteditable = "contenteditable"
    show Controls = "controls"
    show Coords = "coords"
    show Crossorigin = "crossorigin"
    show Data = "data"
    show Datetime = "datetime"
    show Decoding = "decoding"
    show Default = "default"
    show Defer = "defer"
    show Dir = "dir"
    show Dirname = "dirname"
    show Disabled = "disabled"
    show Download = "download"
    show Draggable = "draggable"
    show Enctype = "enctype"
    show Enterkeyhint = "enterkeyhint"
    show Fetchpriority = "fetchpriority"
    show For = "for"
    show Form = "form"
    show Formaction = "formaction"
    show Formenctype = "formenctype"
    show Formmethod = "formmethod"
    show Formnovalidate = "formnovalidate"
    show Formtarget = "formtarget"
    show Headers = "headers"
    show Headingoffset = "headingoffset"
    show Headingreset = "headingreset"
    show Height = "height"
    show Hidden = "hidden"
    show High  = "high"
    show Href = "href"
    show Hreflang = "hreflang"
    show HttpEquiv = "http-equiv"
    show Id = "id"
    show Imagesizes = "imagesizes"
    show Imagesrcset = "imagesrcset"
    show Inert = "inert"
    show Inputmode = "inputmode"
    show Integrity = "integrity"
    show Is = "is"
    show Ismap = "ismap"
    show Itemid = "itemid"
    show Itemprop = "itemprop"
    show Itemref = "itemref"
    show Itemscope = "itemscope"
    show Itemtype = "itemtype"
    show Kind = "kind"
    show Label = "label"
    show Lang = "lang"
    show List = "list"
    show Loading = "loading"
    show Loop = "loop"
    show Low = "low"
    show Max = "max"
    show Maxlength = "maxlength"
    show Media = "media"
    show Method = "method"
    show Min = "min"
    show Minlength = "minlength"
    show Multiple = "multiple"
    show Muted = "muted"
    show Name = "name"
    show Nonce = "nonce"
    show Nomodule = "nomodule"
    show Novalidate = "novalidate"
    show Open = "open"
    show Optimum = "optimum"
    show Pattern = "pattern"
    show Ping = "ping"
    show Placeholder = "placeholder"
    show Playsinline = "playsinline"
    show Popover = "popover"
    show Popovertarget = "popovertarget"
    show Popovertargetaction = "popovertargetaction"
    show Poster = "poster"
    show Preload = "preload"
    show Readonly = "readonly"
    show Referrerpolicy = "referrerpolicy"
    show Rel = "rel"
    show Required = "required"
    show Reversed = "reversed"
    show Rows = "rows"
    show Rowspan = "rowspan"
    show Sandbox = "sandbox"
    show Scope = "scope"
    show Selected = "selected"
    show Shadowrootmode = "shadowrootmode"
    show Shadowrootdelegatesfocus = "shadowrootdelegatesfocus"
    show Shadowrootslotassignment = "shadowrootslotassignment"
    show Shadowrootclonable = "shadowrootclonable"
    show Shadowrootserializable = "shadowrootserializable"
    show Shadowrootcustomelementregistry = "shadowrootcustomelementregistry"
    show Shape = "shape"
    show Size = "size"
    show Sizes = "sizes"
    show Slot = "slot" 
    show Span = "span"
    show Spellcheck = "spellcheck"
    show Src = "src"
    show Srcdoc = "srcdoc"
    show Srclang = "srclang"
    show Srcset = "srcset"
    show Start = "start"
    show Step = "step"
    show Style = "style"
    show Tabindex = "tabindex"
    show Target = "target"
    show Title = "title"
    show Translate = "translate"
    show Type' = "type"
    show Usemap = "usemap"
    show Value = "value"
    show Width = "width"
    show Wrap = "wrap"
    show Writingsuggestions = "writingsuggestions"
    show Onauxclick = "onauxclick"
    show Onbeforeinput = "onbeforeinput"
    show Onbeforematch = "onbeforematch"
    show Onbeforetoggle = "onbeforetoggle"
    show Onblur = "onblur"
    show Oncancel = "oncancel"
    show Oncanplay = "oncanplay"
    show Oncanplaythrough = "oncanplaythrough"
    show Onchange = "onchange"
    show Onclick = "onclick"
    show Onclose = "onclose"
    show Oncommand = "oncommand"
    show Oncontextlost = "oncontextlost"
    show Oncontextmenu = "oncontextmenu"
    show Oncontextrestored = "oncontextrestored"
    show Oncopy = "oncopy"
    show Oncuechange = "oncuechange"
    show Oncut = "oncut"
    show Ondblclick = "ondblclick"
    show Ondrag = "ondrag"
    show Ondragend = "ondragend"
    show Ondragenter = "ondragenter"
    show Ondragleave = "ondragleave"
    show Ondragover = "ondragover"
    show Ondragstart = "ondragstart"
    show Ondrop = "ondrop"
    show Ondurationchange = "ondurationchange"
    show Onemptied = "onemptied"
    show Onended = "onended"
    show Onerror = "onerror"
    show Onfocus = "onfocus"
    show Onformdata = "onformdata"
    show Oninput = "oninput"
    show Oninvalid = "oninvalid"
    show Onkeydown = "onkeydown"
    show Onkeypress = "onkeypress"
    show Onkeyup = "onkeyup"
    show Onload = "onload"
    show Onloadeddata = "onloadeddata"
    show Onloadedmetadata = "onloadedmetadata"
    show Onloadstart = "onloadstart"
    show Onmousedown = "onmousedown"
    show Onmouseenter = "onmouseenter"
    show Onmouseleave = "onmouseleave"
    show Onmousemove = "onmousemove"
    show Onmouseout = "onmouseout"
    show Onmouseover = "onmouseover"
    show Onmouseup = "onmouseup"
    show Onpaste = "onpaste"
    show Onpause = "onpause"
    show Onplay = "onplay"
    show Onplaying = "onplaying"
    show Onprogress = "onprogress"
    show Onratechange = "onratechange"
    show Onreset = "onreset"
    show Onresize = "onresize"
    show Onscroll = "onscroll"
    show Onscrollend = "onscrollend"
    show Onsecuritypolicyviolation = "onsecuritypolicyviolation"
    show Onseeked = "onseeked"
    show Onseeking = "onseeking"
    show Onselect = "onselect"
    show Onslotchange = "onslotchange"
    show Onstalled = "onstalled"
    show Onsubmit = "onsubmit"
    show Onsuspend = "onsuspend"
    show Ontimeupdate = "ontimeupdate"
    show Ontoggle = "ontoggle"
    show Onvolumechange = "onvolumechange"
    show Onwaiting = "onwaiting"
    show Onwheel = "onwheel"
    show Onafterprint = "onafterprint"
    show Onbeforeprint = "onbeforeprint"
    show Onbeforeunload = "onbeforeunload"
    show Onhashchange = "onhashchange"
    show Onlanguagechange = "onlanguagechange"
    show Onmessage = "onmessage"
    show Onmessageerror = "onmessageerror"
    show Onoffline = "onoffline"
    show Ononline = "ononline"
    show Onpagehide = "onpagehide"
    show Onpagereveal = "onpagereveal"
    show Onpageshow = "onpageshow"
    show Onpageswap = "onpageswap"
    show Onpopstate = "onpopstate"
    show Onrejectionhandled = "onrejectionhandled"
    show Onstorage = "onstorage"
    show Onunhandledrejection = "onunhandledrejection"
    show Onunload = "onunload"
    show (Custom str) = str