||| Defines the valid attributes in HTML and the different formats they can have
module Core.Attributes

import Derive.Prelude
import Deriving.Show

%language ElabReflection

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


%runElab derive "Attribute" [Eq]

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
    Cors = "anonymous" || "use-credentials" || ""

    public export
    Preload : Format
    Preload = "auto" || "none" || "metadata" || ""

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

||| Derived implementation of Show. It keeps the same casing and doesn't
||| generate the correct string for attributes with "-" in the name
public export
showDefault : Show Attribute
showDefault = %runElab derive

public export
Show Attribute where
    show HttpEquiv = "http-equiv"
    show AcceptCharset = "accept-charset"
    show attr = toLower (show @{showDefault} attr)