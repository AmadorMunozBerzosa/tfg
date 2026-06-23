||| Functions for parsing HTML documents in JSON into the `Node` type
module Cli.Parsing 

import Language.JSON.Data
import Implementation
import DSL

||| Parses a String into an element tag.
||| If it doesn't recognise it, it assumes it's a custom tag
||| (even though it may not be valid)
tag: String -> Tag
tag "html" = Html
tag "head"  = Head
tag "title"  = Title
tag "base"  = Base
tag "link"  = Link
tag "meta"  = Meta
tag "style"  = Style
tag "body" = Body
tag "article" = Article
tag "section" = Section
tag "nav" = Nav
tag "aside" = Aside
tag "h1" = H1
tag "h2" = H2
tag "h3" = H3
tag "h4" = H4
tag "h5" = H5
tag "h6" = H6
tag "hgroup" = Hgroup
tag "header" = Header
tag "footer" = Footer
tag "address" = Address
tag "p" = P
tag "hr" = Hr
tag "pre" = Pre
tag "blockquote" = Blockquote
tag "ol" = Ol
tag "ul" = Ul
tag "menu" = Menu
tag "li" = Li
tag "dl" = Dl
tag "dt" = Dt
tag "dd" = Dd
tag "figure" = Figure
tag "figcaption" = Figcaption
tag "main" = Main
tag "search" = Search
tag "div" = Div
tag "a" = A
tag "em" = Em
tag "strong" = Strong
tag "small" = Small
tag "s" = S
tag "cite" = Cite
tag "q" = Q
tag "dfn" = Dfn
tag "abbr" = Abbr
tag "ruby" = Ruby
tag "rt" = Rt
tag "rp" = Rp
tag "data" = Data
tag "time" = Time
tag "code" = Code
tag "var" = Var
tag "samp" = Samp
tag "kbd" = Kbd
tag "sub" = Sub
tag "sup" = Sup
tag "i" = I
tag "b" = B
tag "u" = U
tag "mark" = Mark
tag "bdi" = Bdi
tag "bdo" = Bdo
tag "span" = Span
tag "br" = Br
tag "wbr" = Wbr
tag "ins" = Ins
tag "del" = Del
tag "picture" = Picture
tag "source" = Source
tag "img" = Img
tag "iframe" = Iframe
tag "embed" = Embed
tag "object" = Object
tag "video" = Video
tag "audio" = Audio
tag "track" = Track
tag "map" = Map
tag "area" = Area
tag "math" = Math
tag "svg" = SVG
tag "table" = Table
tag "caption" = Caption
tag "colgroup" = Colgroup
tag "col" = Col
tag "tbody" = Tbody
tag "thead" = Thead
tag "tfoot" = Tfoot
tag "tr" = Tr
tag "td" = Td
tag "th" = Th
tag "form" = Form
tag "label" = Label
tag "input" = Input
tag "button" = Button
tag "select" = Select
tag "datalist" = Datalist
tag "optgroup" = Optgroup
tag "option" = Option
tag "textarea" = Textarea
tag "output" = Output
tag "progress" = Progress
tag "meter" = Meter
tag "fieldset" = Fieldset
tag "legend" = Legend
tag "selectedcontent" = SelectedContent
tag "details" = Details
tag "summary" = Summary
tag "dialog" = Dialog
tag "script" = Script
tag "noscript" = Noscript
tag "template" = Template
tag "slot" = Slot
tag "canvas" = Canvas
tag str = Custom str

||| Parses a String into an attribute name.
||| If it doesn't recognise it, it assumes it's a custom attribute
||| (even though it may not be valid)
attribute: String -> Attribute
attribute "accesskey" = Accesskey
attribute "abbr" = Abbr
attribute "accept" = Accept
attribute "accept-charset" = AcceptCharset
attribute "action" = Action
attribute "allow" = Allow
attribute "allowfullscreen" = Allowfullscreen
attribute "alpha" = Alpha
attribute "alt" = Alt
attribute "as" = As
attribute "async" = Async
attribute "autocapitalize" = Autocapitalize
attribute "autocomplete" = Autocomplete
attribute "autocorrect" = Autocorrect
attribute "autofocus" = Autofocus
attribute "autoplay" = Autoplay
attribute "blocking" = Blocking
attribute "charset" = Charset
attribute "checked" = Checked
attribute "cite" = Cite
attribute "class" = Class
attribute "closedBy" = ClosedBy
attribute "color" = Color
attribute "colorspace" = Colorspace
attribute "cols" = Cols
attribute "colspan" = Colspan
attribute "command" = Command
attribute "commandfor" = Commandfor
attribute "content" = Content
attribute "contenteditable" = Contenteditable
attribute "controls" = Controls
attribute "coords" = Coords
attribute "crossorigin" = Crossorigin
attribute "data" = Data
attribute "datetime" = Datetime
attribute "decoding" = Decoding
attribute "default" = Default
attribute "defer" = Defer
attribute "dir" = Dir
attribute "dirname" = Dirname
attribute "disabled" = Disabled
attribute "download" = Download
attribute "draggable" = Draggable
attribute "enctype" = Enctype
attribute "enterkeyhint" = Enterkeyhint
attribute "fetchpriority" = Fetchpriority
attribute "for" = For
attribute "form" = Form
attribute "formaction" = Formaction
attribute "formenctype" = Formenctype
attribute "formmethod" = Formmethod
attribute "formnovalidate" = Formnovalidate
attribute "formtarget" = Formtarget
attribute "headers" = Headers
attribute "headingoffset" = Headingoffset
attribute "headingreset" = Headingreset
attribute "height" = Height
attribute "hidden" = Hidden
attribute  "high" = High
attribute "href" = Href
attribute "hreflang" = Hreflang
attribute "http-equiv" = HttpEquiv
attribute "id" = Id
attribute "imagesizes" = Imagesizes
attribute "imagesrcset" = Imagesrcset
attribute "inert" = Inert
attribute "inputmode" = Inputmode
attribute "integrity" = Integrity
attribute "is" = Is
attribute "ismap" = Ismap
attribute "itemid" = Itemid
attribute "itemprop" = Itemprop
attribute "itemref" = Itemref
attribute "itemscope" = Itemscope
attribute "itemtype" = Itemtype
attribute "kind" = Kind
attribute "label" = Label
attribute "lang" = Lang
attribute "list" = List
attribute "loading" = Loading
attribute "loop" = Loop
attribute "low" = Low
attribute "max" = Max
attribute "maxlength" = Maxlength
attribute "media" = Media
attribute "method" = Method
attribute "min" = Min
attribute "minlength" = Minlength
attribute "multiple" = Multiple
attribute "muted" = Muted
attribute "name" = Name
attribute "nonce" = Nonce
attribute "nomodule" = Nomodule
attribute "novalidate" = Novalidate
attribute "open" = Open
attribute "optimum" = Optimum
attribute "pattern" = Pattern
attribute "ping" = Ping
attribute "placeholder" = Placeholder
attribute "playsinline" = Playsinline
attribute "popover" = Popover
attribute "popovertarget" = Popovertarget
attribute "popovertargetaction" = Popovertargetaction
attribute "poster" = Poster
attribute "preload" = Preload
attribute "readonly" = Readonly
attribute "referrerpolicy" = Referrerpolicy
attribute "rel" = Rel
attribute "required" = Required
attribute "reversed" = Reversed
attribute "rows" = Rows
attribute "rowspan" = Rowspan
attribute "sandbox" = Sandbox
attribute "scope" = Scope
attribute "selected" = Selected
attribute "shadowrootmode" = Shadowrootmode
attribute "shadowrootdelegatesfocus" = Shadowrootdelegatesfocus
attribute "shadowrootslotassignment" = Shadowrootslotassignment
attribute "shadowrootclonable" = Shadowrootclonable
attribute "shadowrootserializable" = Shadowrootserializable
attribute "shadowrootcustomelementregistry" = Shadowrootcustomelementregistry
attribute "shape" = Shape
attribute "size" = Size
attribute "sizes" = Sizes
attribute "slot"  = Slot
attribute "span" = Span
attribute "spellcheck" = Spellcheck
attribute "src" = Src
attribute "srcdoc" = Srcdoc
attribute "srclang" = Srclang
attribute "srcset" = Srcset
attribute "start" = Start
attribute "step" = Step
attribute "style" = Style
attribute "tabindex" = Tabindex
attribute "target" = Target
attribute "title" = Title
attribute "translate" = Translate
attribute  "type" = Type'
attribute "usemap" = Usemap
attribute "value" = Value
attribute "width" = Width
attribute "wrap" = Wrap
attribute "writingsuggestions" = Writingsuggestions
attribute "onauxclick" = Onauxclick
attribute "onbeforeinput" = Onbeforeinput
attribute "onbeforematch" = Onbeforematch
attribute "onbeforetoggle" = Onbeforetoggle
attribute "onblur" = Onblur
attribute "oncancel" = Oncancel
attribute "oncanplay" = Oncanplay
attribute "oncanplaythrough" = Oncanplaythrough
attribute "onchange" = Onchange
attribute "onclick" = Onclick
attribute "onclose" = Onclose
attribute "oncommand" = Oncommand
attribute "oncontextlost" = Oncontextlost
attribute "oncontextmenu" = Oncontextmenu
attribute "oncontextrestored" = Oncontextrestored
attribute "oncopy" = Oncopy
attribute "oncuechange" = Oncuechange
attribute "oncut" = Oncut
attribute "ondblclick" = Ondblclick
attribute "ondrag" = Ondrag
attribute "ondragend" = Ondragend
attribute "ondragenter" = Ondragenter
attribute "ondragleave" = Ondragleave
attribute "ondragover" = Ondragover
attribute "ondragstart" = Ondragstart
attribute "ondrop" = Ondrop
attribute "ondurationchange" = Ondurationchange
attribute "onemptied" = Onemptied
attribute "onended" = Onended
attribute "onerror" = Onerror
attribute "onfocus" = Onfocus
attribute "onformdata" = Onformdata
attribute "oninput" = Oninput
attribute "oninvalid" = Oninvalid
attribute "onkeydown" = Onkeydown
attribute "onkeypress" = Onkeypress
attribute "onkeyup" = Onkeyup
attribute "onload" = Onload
attribute "onloadeddata" = Onloadeddata
attribute "onloadedmetadata" = Onloadedmetadata
attribute "onloadstart" = Onloadstart
attribute "onmousedown" = Onmousedown
attribute "onmouseenter" = Onmouseenter
attribute "onmouseleave" = Onmouseleave
attribute "onmousemove" = Onmousemove
attribute "onmouseout" = Onmouseout
attribute "onmouseover" = Onmouseover
attribute "onmouseup" = Onmouseup
attribute "onpaste" = Onpaste
attribute "onpause" = Onpause
attribute "onplay" = Onplay
attribute "onplaying" = Onplaying
attribute "onprogress" = Onprogress
attribute "onratechange" = Onratechange
attribute "onreset" = Onreset
attribute "onresize" = Onresize
attribute "onscroll" = Onscroll
attribute "onscrollend" = Onscrollend
attribute "onsecuritypolicyviolation" = Onsecuritypolicyviolation
attribute "onseeked" = Onseeked
attribute "onseeking" = Onseeking
attribute "onselect" = Onselect
attribute "onslotchange" = Onslotchange
attribute "onstalled" = Onstalled
attribute "onsubmit" = Onsubmit
attribute "onsuspend" = Onsuspend
attribute "ontimeupdate" = Ontimeupdate
attribute "ontoggle" = Ontoggle
attribute "onvolumechange" = Onvolumechange
attribute "onwaiting" = Onwaiting
attribute "onwheel" = Onwheel
attribute "onafterprint" = Onafterprint
attribute "onbeforeprint" = Onbeforeprint
attribute "onbeforeunload" = Onbeforeunload
attribute "onhashchange" = Onhashchange
attribute "onlanguagechange" = Onlanguagechange
attribute "onmessage" = Onmessage
attribute "onmessageerror" = Onmessageerror
attribute "onoffline" = Onoffline
attribute "ononline" = Ononline
attribute "onpagehide" = Onpagehide
attribute "onpagereveal" = Onpagereveal
attribute "onpageshow" = Onpageshow
attribute "onpageswap" = Onpageswap
attribute "onpopstate" = Onpopstate
attribute "onrejectionhandled" = Onrejectionhandled
attribute "onstorage" = Onstorage
attribute "onunhandledrejection" = Onunhandledrejection
attribute "onunload" = Onunload
attribute str = Custom str

||| Tries to extract a string from a JSON value
parseString: JSON -> Maybe String
parseString (JString string) = Just string
parseString _ = Nothing

||| Tries to extract an integer from a JSON value
parseInt: JSON -> Maybe Int
parseInt (JNumber double) = Just(cast double)
parseInt _ = Nothing

||| Tries to extract an array from a JSON value
parseArray: JSON -> Maybe (List JSON)
parseArray (JArray nodes) = Just nodes
parseArray _ = Nothing

||| Tries to extract an object from a JSON value
parseObject: JSON -> Maybe (List (String,JSON))
parseObject (JObject object) = Just object
parseObject _ = Nothing

||| Tries to extract a comment node from an object value
parseComment: List (String, JSON) -> Maybe CData
parseComment object = lookup "content" object >>= parseString |> map Comment

||| Tries to extract a text node from an object value
parseText: List (String, JSON) -> Maybe CData
parseText object = lookup "content" object >>= parseString |> map Text

||| Tries to extract an attribute (name,value) pair from an object value
parseAttribute: JSON -> Maybe (Attribute, String)
parseAttribute node = do
    object <- parseObject node
    key <- lookup "key" object >>= parseString |> map toLower
    let value = lookup "value" object >>= parseString |> fromMaybe ""
    
    Just (attribute key,value)

||| Tries to extract a list of attribute (name,value) pairs from an array value
parseAttributes: List JSON -> Maybe (Map Attribute String)
parseAttributes array = do
    list <- array |> map parseAttribute |> combine
    
    Just (fromList list)

||| Parses a (line,column) pair from a position from an object value
parsePosition: JSON -> Maybe (Int,Int)
parsePosition node = do
    object <- parseObject node
    start <- lookup "start" object >>= parseObject
    line <- lookup "line" start >>= parseInt
    column <- lookup "column" start >>= parseInt
    Just (line, column)

public export
Index: Type
Index = (Int,Int)

public export
Indexed: Type -> Type
Indexed a = (Index,a)

PositionNode: Type
PositionNode = Tree (Indexed CData) (Indexed ElementNode)

||| Parses a JSON object into a Node value
parseNode: JSON -> Maybe PositionNode
parseNode node = do
    object <- parseObject node
    type <- lookup "type" object >>= parseString
    position <- lookup "position" object >>= parsePosition

    case type of
        "comment" => do
            comment <- parseComment object
            Just (Leaf (position,comment))

        "text" => do
            text <- parseText object
            Just (Leaf (position,text))

        "element" => do
            name <- lookup "tagName" object >>= parseString
            
            let childrenJSON = lookup "children" object >>= parseArray |> fromMaybe []
            let attrsJSON = lookup "attributes" object >>= parseArray |> fromMaybe []
            
            children <- childrenJSON |> map parseNode |> combine
            attrs <- attrsJSON |> map parseAttribute |> combine

            let element = Element (tag name) (fromList attrs)

            Just (Branch (position,element) children)
        _ => Nothing

||| Parses the contents of a JSON file into a Node value,
||| together with the positions (line,col) where each node begins
||| It assumes the JSON file has the format specified
||| by the himalaya npm library:
||| https://github.com/andrejewski/himalaya/blob/master/text/ast-spec-v1.md
export
parse: JSON -> Maybe (Tree Index,Node)
parse (JArray xs) = do
    -- If the document contains a DOCTYPE declaration,
    -- it will be parsed by himalaya as two documents
    -- so we take the last element
    x <- last' xs
    tree <- parseNode x
    Just (split tree)
    
parse _ = Nothing