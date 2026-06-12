||| Tools to verify that a HTML element has:
||| 1. Valid attributes
||| 2. Correct values for its attributes
module Verification.Attributes
 
import Core.Tags
import Core.Node
import Core.Specification
import Core.Attributes
import Definitions.Specification
import Util.Map
import Util.Tree
import Util.List
import Util.Application
import Data.Maybe
import Data.List
import Data.String
import Util.Parser

||| Attributes that can be specified on any HTML element
public export
globalAttributes: List Attribute
globalAttributes = [
    -- General global attributes
    Accesskey, Autocapitalize, Autocorrect, Autofocus, Contenteditable, Dir, Draggable, Enterkeyhint, Headingoffset, Headingreset, Hidden, Id, Inert, Inputmode, Is, Itemid, Itemprop, Itemref, Itemscope, Itemtype, Lang, Nonce, Popover, Spellcheck, Style, Tabindex, Title, Translate, Writingsuggestions,
    -- Special global attributes
    Id, Class, Slot,
    -- Event global attributes
    Onauxclick, Onbeforeinput, Onbeforematch, Onbeforetoggle, Onblur, Oncancel, Oncanplay, Oncanplaythrough, Onchange, Onclick, Onclose, Oncommand, Oncontextlost, Oncontextmenu, Oncontextrestored, Oncopy, Oncuechange, Oncut, Ondblclick, Ondrag, Ondragend, Ondragenter, Ondragleave, Ondragover, Ondragstart, Ondrop, Ondurationchange, Onemptied, Onended, Onerror, Onfocus, Onformdata, Oninput, Oninvalid, Onkeydown, Onkeypress, Onkeyup, Onload, Onloadeddata, Onloadedmetadata, Onloadstart, Onmousedown, Onmouseenter, Onmouseleave, Onmousemove, Onmouseout, Onmouseover, Onmouseup, Onpaste, Onpause, Onplay, Onplaying, Onprogress, Onratechange, Onreset, Onresize, Onscroll, Onscrollend, Onsecuritypolicyviolation, Onseeked, Onseeking, Onselect, Onslotchange, Onstalled, Onsubmit, Onsuspend, Ontimeupdate, Ontoggle, Onvolumechange, Onwaiting, Onwheel
]

||| Given an attribute name and a format, it gives a parser that checks
||| whether a value conforms to the format.
||| The attribute name is needed, since Boolean attributes allow their
||| value to be the attribute's name
public export
parser : Attribute -> Format -> Parser String Unit
parser _ Anything = anything

parser _ (Text str') = literal (toLower str')
parser _ (Trimmed str') = whitespace >> literal (toLower str') >> whitespace
parser _ (WhitespaceBetween first last) =
    literal (toLower first) >> whitespace >> literal (toLower last)

parser _ (Prefix pref) = literal (toLower pref) >> anything

parser _ SingleLine = singleLine
parser _ NoWhitespace = singleWord

parser _ NonNegative = ignore natural
parser _ Positive = natural >>= (/= 0) .> guard
parser _ Integer' = ignore integer
parser _ Float = ignore double
parser _ (LessThan num) = double >>= (<= num) .> guard
parser _ (GreaterThan num) = double  >>= (>= num) .> guard

parser _ Character = ignore char
parser name (List separator format) = ignore (list (literal separator) (parser name format))
parser name (Set separator format) = list (literal separator) (keepOriginal (parser name format)) >>= isSet .> guard

parser name Boolean = nothing || literal (show name)

parser _ Size =
    literal "any" .<|>. (atLeastOne digit .>>. literal "x" .>>. atLeastOne digit)
    >>= ( guard .
        \case
            Left () => True
            Right ('0'::_, (), _) => False
            Right (_, (), '0'::_) => False
            Right _ => True
    )
        
-- 
parser _ CircleCoords =
    list (literal " ") integer
    >>= ( guard .
        \case
            [left, top, radius] => radius >= 0
            _ => False
    )

parser _ RectCoords = 
    list (literal " ") integer
    >>= (
        \case
            [first, second, third, fourth] => first < third && second < fourth
            _ => False
    ) .> guard

parser _ PolygonCoords =
    list (literal " ") integer
    >>= (\list => not (null list) && (cast (length list) `mod` 2) == 0) .> guard

parser _ NavigableTargetName = From (\string =>
    if
        ("_" `isInfixOf` string)
        ||
        ("\n" `isInfixOf` string)
        ||
        ("\t" `isInfixOf` string)
        ||
        ("<" `isInfixOf` string)
    then
        Reject
    else
        Accept () ""
    )

parser _ Month = ignore month
parser _ Week = ignore week
parser _ Date = ignore date
parser _ Time = ignore time
parser _ Datetime = ignore localDateTime

parser _ Autocomplete = literal "on" || literal "off" || autoFill

parser atr (a || b) = parser atr a || parser atr b

-- It is out of the scope of this library to parse these formats
-- We include them both for completeness and for the future possibility
-- of warning the user to check these values manually

parser _ URL = anything
parser _ NotURL = anything
parser _ ImageCandidate = anything
parser _ AbsoluteURL = anything
parser _ SrcDoc = anything

parser _ Integrity = anything
parser _ Pattern = anything
parser _ RefreshPolicy = anything
parser _ ContentSecurityPolicy = anything
parser _ PermissionsPolicy = anything
parser _ EventHandler = anything
parser _ LanguageCode = anything
parser _ MimeType = anything
parser _ FileAccept = anything
parser _ Color = anything
parser _ MediaQuery = anything
parser _ Email = anything

||| Returns True if the attribute value conforms to its format
public export
matches : Attribute -> Format -> String -> Bool
matches name format string =
    case run (parser name format) (toLower string) of
        Accept () "" => True 
        Accept () _ => False
        Reject => False
            
||| List of attributes an element is allowed to have
public export
validAttributes : ElementNode -> List Attribute
validAttributes (Element tag _) = globalAttributes ++ (specification tag).attributes

||| List of attributes an element has
public export 
effectiveAttributes : ElementNode -> List Attribute
effectiveAttributes (Element _ attributes) = keys attributes
 
||| Returns True if it's a valid custom attribute (either a data- attribute or an ARIA attribute)
public export
validCustomAttribute : Attribute -> Bool
validCustomAttribute (Custom str) = ("data-" `isPrefixOf` str) || ("aria-" `isPrefixOf` str) || "role" == str
validCustomAttribute _ = False

||| Gives the list of a node's invalid attributes
||| (those that are not global or specific or custom attributes)
public export
validate : Node -> List Attribute
validate (Leaf _ ) = []
validate (Branch (Element (Custom _) _) _) = [] -- Autonomous custom elements can have any attributes
validate (Branch element _) =
    effectiveAttributes element \\ validAttributes element
    |> filter (validCustomAttribute .> not)