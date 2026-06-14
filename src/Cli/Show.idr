||| Functions for printing error trees in a human-readable format
module Cli.Show

import Implementation
import Cli.Parsing

Show Format where
    show Anything = "Anything"
    show (Text string) = show string
    show (Trimmed string) = show string
    show (WhitespaceBetween first last) = show (first ++ " " ++ last)
    show (Prefix string) = "\{show string}-"
    show SingleLine = "No line feeds or carriage returns"
    
    show (LessThan num) = "Smaller than \{show num}"
    show (GreaterThan num) = "Greater than \{show num}"
    show NonNegative = "Non-negative integer"
    show Positive = "Positive integer"
    show Integer' = "Integer"
    show Float = "Floating-point Integer'"

    show Character = "Single character"
    show (List separator format) = "List of (\{show format}) separated by \"\{separator}\""
    show (Set separator format) = "Set of (\{show format}) separated by \"\{separator}\""
    show Boolean = "Either the empty string or the name of the attribute"
    show Date = "A YYYY/MM/DD date string"
    show Month = "A month number (between 1 and 12)"
    show Week = "A week number (between 1 and 53)"
    show Time = "A hh:mm:ss.ddd datetime string"
    show Datetime = "A YYYY/MM/DDThh:mm:ss.ddd datetime string"
    show Size = "A XxY size string"

    show CircleCoords = "Circle coordinates"
    show RectCoords = "Rectangle coordinates"
    show PolygonCoords = "Polygon coordinates"

    show NavigableTargetName = "Navigable target name (not containing newlines, tabulators, \"_\" or \"<\""

    show NotURL = "Not URL"
    show URL = "URL"
    show AbsoluteURL = "Absolute URL"
    show Email = "Email address"
    show LanguageCode = "Language code"
    show MimeType = "MIME type"
    show SrcDoc = "HTML source documnt"
    show EventHandler = "Event handler"
    show Color = "Color"
    show ImageCandidate = "Image candidate"
    show FileAccept = "File accept format"
    show Pattern = "JS Regex"
    show Autocomplete = "Autocomplete value"
    show Integrity = "Integrity metadata"
    show NoWhitespace = "No whitespace"

    show MediaQuery = "Media query"
    show PermissionsPolicy = "Permissions policy"
    show ContentSecurityPolicy = "Content Security Policy"
    show RefreshPolicy = "Refresh policy"
    show (format || format') = "\{show format} | \{show format'}"


Show Category where
    show Metadata = "Metadata"
    show Flow = "Flow"
    show Sectioning = "Sectioning"
    show Heading = "Heading"
    show Phrasing = "Phrasing"
    show Embedded = "Embedded"
    show Interactive = "Interactive"
    show Palpable = "Palpable"
    show FormAssociated = "Form-associated"
    show ScriptSupporting = "Script-supporting"
    show SelectElementInnerContent = "Select element inner content"
    show OptgroupElementInnerContent = "Optgroup element inner content"
    show OptionElementInnerContent = "Option element inner content"
    show Listed = "Listed"
    show Labelable = "Labelable"
    show Submitable = "Submitable"
    show Resettable = "Resettable"
    show AutocapitalizeAndAutocorrectInheriting = "Autocapitalize-and-autocorrect-inheriting"

Show Condition where
    -- Combinators
    show True = ""
    show (c && True) = show c
    show (c && c') = "\{show c}, \{show c'}"
    show (a ==> b) = "\{show a} ==> \{show b}"
    show (a || b) = "\{show a} | \{show b}"
    -- Basic conditions
    show (Tag tag) = "<\{show tag}>"
    show (Category category) = "\{show category} element"
    show HasContent = "Not empty"
    show Childless = "No element children"
    show ValidTagName = "Valid tagname"
    -- Attribute presence
    show (Has attr) = "Has the \"\{show attr}\" attribute"
    show (HasNot attr) = "Doesn't have the \"\{show attr}\" attribute"
    show (HasAny attrs) = "Has at least one of the following attributes: \{joinBy ", " (map show attrs)}"
    show (HasNone attrs) = "Has none of the following attributes: \{joinBy ", " (map show attrs)}"
    show (HasAtMostOne attrs) = "Has at most one of the following attributes: \{joinBy ", " (map show attrs)}"
    -- Attribute values
    show (NotEmpty attr) = "Attribute \"\{show attr}\" has a non-empty value"
    show (Is attr format) = "Attribute \"\{show attr}\" is \{show format}"
    show (IsRequired attr format) = "Attribute \"\{show attr}\" is \{show format}"
    show (IsNot attr string) = "Attribute \"\{show attr}\" is not \{string}"
    show (IsRequiredNot attr format) = "Attribute \"\{show attr}\" is not \{show format}"
    show (IncludesAny attr [value]) = "Attribute \"\{show attr}\" contains the \{value} value"
    show (IncludesAny attr values) = "Attribute \"\{show attr}\" contains one of following values: \{join ", " values}"
    show (IncludesRequired attr [value]) = "Attribute \"\{show attr}\" contains the \{value} value"
    show (IncludesRequired attr values) = "Attribute \"\{show attr}\" contains one of following values: \{join ", " values}"
    show (IncludesNone attr [value]) = "Attribute \"\{show attr}\" doesn't contain the \{value} value"
    show (IncludesNone attr values) = "Attribute \"\{show attr}\" contains none of following values: \{join ", " values}"
    show (Same atr atr') = "The \"\{show atr}\" and \"\{show atr'}\" attributes have the same value"
    show (Condition.(<=) str str') = "\"\{show str}\" <= \"\{show str'}\""
    show (Condition.(>=) str str') = "\"\{show str}\" >= \"\{show str'}\""
    -- Attributes values within the context of the document
    show (Unique attr) = "The \"\{show attr}\" attribute is unique within the document"
    show (UniqueTag attr) = "The \"\{show attr}\" attribute is unique among elements with the same tag"
    show (References attr attr') = "The \"\{show attr}\" attribute matches the \"\{show attr'}\" attribute of an element"
    show (ReferencesTag attr (tag, attr')) = "The \"\{show attr}\" attribute matches \"\{show attr'}\" attribute of a <\{show tag}> tag"
    show (ReferencesCategory attr (category, attr')) = "The \"\{show attr}\" attribute matches the \"\{show attr'}\" attribute of a \{show category} element"
    show (ReferencesAttribute attr (attribute, attr')) = "The \"\{show attr}\" attribute matches the \"\{show attr'}\" attribute of an element with the \"\{show attribute}\" attribute"
    show (EachReferencesId attr) = "All values in the \"\{show attr}\" attribute match the \"id\" attribute of other elements"
    show (EachReferencesTh attr) = "All values in the \"\{show attr}\" attribute match the \"id\" of any <th> tag in the same table"
    -- Parent element
    show (HasParent tag) = "Parent is a <\{show tag}> tag"
    show (NotParent tag) = "Parent is not a <\{show tag}> tag"
    -- Next sibling
    show NextSiblingAuto = "The next element is an image that allows for auto-zising"
    show (NextSiblingTag tags) = "The next element is any of the following tags: \{tags |> map show |> join " | " }"
    -- Ancestor elements
    show (HasAncestor tag) = "Has a <\{show tag}> ancestor"
    show (HasAncestorAttribute tag attribute) = "Has a <\{show tag}> ancestor with the \"\{show attribute}\" attribute"
    show (NoAncestor tag) = "Has no <\{show tag}> ancestors"
    -- Child elements
    show (HasChild tag) = "Has a <\{show tag}> child"
    show (HasChildCategory cat) = "Has a \{show cat} child"
    show (NoChild tag) = "Doesn't have any <\{show tag}> children"
    show (UniqueChild tag) = "Has at most one <\{show tag}> child"
    show UniqueDefaultSubtitles = "Has at most one <track kind=\"subtitles\"> or <track kind=\"caption\"> child with the \"default\" attribute"
    show UniqueDefaultDescription = "Has at most one <track kind=\"description\"> child with the \"default\" attribute"

    show (HasDescendant tag) = "Has a <\{show tag}> descendant"
    show (HasDescendantCategory cat) = "Has a \{show cat} descendant"
    show (NoDescendant tag) = "Doesn't have any <\{show tag}> descendants"
    show (NoIndirectDescendant tag) = "Its children don't have any <\{show tag}> descendants"
    show (NoDescendantCategory cat) = "Doesn't have any \{show cat} descendants"
    show (NoDescendantAttribute attr) = "Doesn't have any descendants with the \"\{show attr}\" attribute"
    show (UniqueDescendantCategory cat) = "Has at most one \{show cat} descendant"
    show (UniqueDescendantAttribute tag attr) = "Has at most one <\{show tag}> descendant with the \"\{show attr}\" attribute"
    -- Special cases
    show BeforeURLs = "Appears before any URLs"
    show UniqueCharset = "Has at most one <meta> tag with the \"charset\" attribute"
    show UniqueTranslationPerLanguage = "Has at most one <meta> tag with the \"application-name\" attribute per value of the  \"lang\" attribute"
    show UniqueDescription = "Has at most one <meta> tag with the \"name=description\" attribute"
    show UniqueThemeColorPerMedia = "Has at most one <meta> tag with the \"name=theme-color\" attribute per value of the \"media\" attribute"
    show UniqueColorScheme = "Has at most one <meta> tag with the \"name=color-scheme\" attribute"
    show UniqueEncoding = "Has at most one <meta> tag with (1) the \"http-equiv=content-type\" attribute or (2) the \"name=charset\" attribute"
    show UniqueMetaState = "Has at most one <meta> tag tag with the same value of the \"http-equiv\" attribute"
    show UniqueTrackPerLangAndLabel = "Has at most one <track> element with the same values in the \"kind\", \"srclang\" and \"label\" attributes, for the same <audio>|<video> element"
    show CorrectHeadingLevel = "Has a valid heading outline (https://html.spec.whatwg.org/multipage/sections.html#headings-and-outlines-2)"
    show CorrectTableModel = "Has a correct table model (https://html.spec.whatwg.org/multipage/tables.html#table-model)"
    show HierarchicallyCorrectMain = "Has an element whose only ancestors are <html>, <body>, <div>, <form> elements an accesible name and autonomous custom elements"
    show OtherRadioGroupOptions = "Has at least two <input type=\"radio\"> elements with the same \"name\" atrribute within the same form"
    show HasPlaceholderLabelOption = "A <option value=\"\"> as its first option"
    show UniqueOpenPerGroup = "A single <details> element with the \"open\" attribute in the same \"name\" group"
    show NotNestedSameName = "A <details> element not nested within another <details> element with the same name"

Show ConditionError where
    -- Combinators
    show (a ==> b) = "\{show b} (The rule applies because: \{show a})"
    show (a || b) = "\{show a} | \{show b}"
    -- Basic conditions
    show (ExpectedTag tag) = "Expected <\{show tag}> tag"
    show (ExpectedCategory cat) = "Expected \{show cat} element"
    show NoContent = "Element mustn't be empty"
    show HasChildren = "Shouldn't have any children"
    show (InvalidTagName string) = "Invalid tagname \{string}"
    -- Attribute presence
    show (ShouldHave attr) = "Missing \"\{show attr}\" attribute"
    show (ShouldNotHave attr) = "Mustn't have the \"\{show attr}\" attribute"
    show (ShouldHaveAny attrs) = "Must have one of the following attributes: \{joinBy ", " (map show attrs)}"
    show (HasMultiple attrs) = "Should only have one of the following attributes: \{joinBy ", " (map show attrs)}"
    -- Attribute values
    show (Empty attr) = "The value of the \"\{show attr}\" can't be empty"
    show (Is attr string) = "The value of the attribute \"\{show attr}\" can't be \{string}"
    show (Is' attr format value) = "The value of the attribute \"\{show attr}\" can't be of the form: \{show format}. Found \"\{value}\""
    show (IsNot attr format value) = "The value of the \"\{show attr}\" must be of the form: \{show format}. Found: \"\{value}\""
    show (DoesNotIncludeAny attr [value]) = "Attribute \"\{show attr}\" must contain the \{value} value"
    show (DoesNotIncludeAny attr values) = "Attribute \"\{show attr}\" must contain any of the following values: \{join ", " values}"
    show (Includes attr value) = "Attribute \"\{show attr}\" mustn't contain the \{value} value"
    show (Different atr atr') = "The \"\{show atr}\" and \"\{show atr'}\" attributes have different values"
    show (str > str') = "\"\{show str}\" > \"\{show str'}\""
    show (str < str') = "\"\{show str}\" < \"\{show str'}\""
    -- Attributes values within the context of the document
    show (NotUnique attr value _) = "There are multiple elements with \"\{show attr}=\{value}\""
    show (NotUniqueTag attr value Nothing _) = "There are multiple elements with \"\{show attr}=\{value}\""
    show (NotUniqueTag attr value (Just tag) _) = "There are multiple <\{show tag}> elements with the same tag and \"\{show attr}=\{value}\""
    show (NoReference attr attr' value) = "There aren't any elements with \"\{show attr'}\=\{value}\""
    show (NoReferenceTag attr (tag, attr') value) = "There aren't any <\{show tag}> elements with \"\{show attr'}\=\{value}\""
    show (NoReferenceCategory attr (cat, attr') value) = "There aren't any \{show cat} elements with \"\{show attr'}\=\{value}\""
    show (NoReferenceAttribute attr (attr'', attr') value) = "There aren't any elements with \"\{show attr'}\=\{value}\" and the \"\{show attr''}\" attribute"
    show (NotEachReferencesId attr [value]) = "The \"\{value}\" value in the \"\{show attr}\" attribute doesn't match the \"id\" attribute of any other element"
    show (NotEachReferencesId attr list) = "The following values in the \"\{show attr}\" attribute don't match the \"id\" attribute of any other element: \{join ", " list}"
    show (NotEachReferencesTh attr [value]) = "The \"\{value}\" value in the \"\{show attr}\" attribute doesn't match the \"id\" attribute of any <th> element in the same table"
    show (NotEachReferencesTh attr list) = "The following values in the \"\{show attr}\" attribute don't match the \"id\" attribute of any <th> element in the same table: \{join ", " list}"
    -- Parent element
    show (WrongParent tag tag') = "Parent must be a <\{show tag}> tag. Found <\{show tag'}>"
    show (ShouldNotHaveParent tag) = "Parent must not be a <\{show tag}>"
    -- Next sibling
    show NotNextSiblingAuto = "There is no next element, or it is not an image that allows for auto-zising"
    show (NextSiblingIsNotAny tags) = "There is no next element, or it is not any of the following tags: \{tags |> map show |> join " | " }"
    -- Ancestor elements
    show (NoAncestor tag) = "Doesn't have any <\{show tag}> ancestors"
    show (HasAncestor tag _) = "Mustn't have a <\{show tag}> ancestor"
    show (NoAncestorAttribute tag attr) = "Doesn't have any <\{show tag}> ancestors with the \"\{show attr}\" attribute"
    -- Child elements
    show (NoChild tag) = "Doesn't have any <\{show tag}> children"
    show (NoChildCategory cat) = "Doesn't have any \{show cat} children"
    show (HasChildrenTag tag _) = "Mustn't have any <\{show tag}> children"
    show (MultipleChild tag _) = "Mustn't have more than one <\{show tag}> child"
    show (MultipleDefaultSubtitles _) = "Has multiple <track kind=\"subtitles\"> or <track kind=\"caption\"> children with the \"default\" attribute"
    show (MultipleDefaultDescription _) = "Has multiple <track kind=\"description\"> children with the \"default\" attribute"

    show (NoDescendant tag) = "Has no <\{show tag}> descendants"
    show (NoDescendantCategory cat) = "Has no \{show cat} descendants"
    show (HasDescendant tag _) = "Mustn't have any <\{show tag}> descendants"
    show (HasIndirectDescendant tag _) = "Its children mustn't have any <\{show tag}> descendants"
    show (HasDescendantCategory cat _) = "Mustn't have any \{show cat} descendants"
    show (HasDescendantAttribute attr _) = "Mustn't have any descendants with the \"\{show attr}\" attribute"
    show (MultipleDescendantCategory cat _) = "Musnt't have more than one \{show cat} descendants"
    show (MultipleDescendantAttribute tag attr _) = "Mustn't have more than one <\{show tag}> descendants with the \"\{show attr}\" attribute"
    -- Special cases
    show (URLsBeforeBase _) = "The <base> element appears after a URLs"
    show (MultipleCharset _) = "Has multiple <meta> tags with the \"charset\" attribute"
    show (MultipleTranslationPerLanguage) = "Has multiple <meta> tags with the \"application-name\" attribute per value of the  \"lang\" attribute"
    show (MultipleDescription) = "Has multiple <meta> tags with the \"name=description\" attribute"
    show (MultipleThemeColorPerMedia) = "Has multiple <meta> tags with the \"name=theme-color\" attribute per value of the \"media\" attribute"
    show (MultipleColorScheme) = "Has multiple <meta> tags with the \"name=color-scheme\" attribute"
    show (MultipleEncoding) = "Has multiple <meta> tags with (1) the \"http-equiv=content-type\" attribute or (2) the \"name=charset\" attribute"
    show (MultipleMetaState) = "Has multiple <meta> tags tag with the same value of the \"http-equiv\" attribute"
    show (MultipleTrackPerLangAndLabel) = "Has multiple <track> element with the same values in the \"kind\", \"srclang\" and \"label\" attributes, for the same <audio>|<video> element"
    show (IncorrectHeadingLevel) = "<h1>-<h6> headings are badly nested (https://html.spec.whatwg.org/multipage/sections.html#headings-and-outlines-2)"
    show (IncorrectTableModel) = "The table model is incorrect (https://html.spec.whatwg.org/multipage/tables.html#table-model)"
    show (HierarchicallyIncorrectMain _) = "The only ancestors of this element should be <html>, <body>, <div>, <form> elements an accesible name and autonomous custom elements"
    show (NoOtherRadioGroupOptions) = "There is no other <input type=\"radio\"> element with the same \"name\" atrribute within the same form"
    show (DoesNotHavePlaceholderLabelOption) = "Must have a <option value=\"\"> tag as its first option"
    show (MultipleOpenPerGroup _) = "There is another <details> element with the \"open\" attribute in the same \"name\" group"
    show (NestedSameName _) = "The <details> element is nested within another <details> element with the same name"

-- Returns True if the string representation of the model is complex
-- (and thus needs parenthesis when mixed with combinators)
isCompound: ContentModel -> Bool
isCompound (Transparent Nothing) = False
isCompound (Transparent _) = True
isCompound (Category _) = True
isCompound (Sequence (_::_::_)) = True
isCompound (Any (_::_::_)) = True
isCompound _ = False 


Show ContentModel where
    show Anything = "Anything"
    show Nothing = "Nothing"
    show Text = "Text"
    show (Tag tag) = "<\{show tag}>"
    show (Category category) = "\{show category} element"
    
    show (Optional model) =
        if isCompound model then
            "(\{show model})?"
        else
            "\{show model}?"

    show (Many model) =
        if isCompound model then
            "(\{show model})*"
        else
            "\{show model}*"

    show (AtLeastOne model) =
        if isCompound model then
            "(\{show model})+"
        else
            "\{show model}+"

    show (Intermixed category model) =
        "\{show model}, optionally intermixed with \{show category} elements"

    show (Sequence []) = ""
    show (Sequence [m]) = show m
    show (Sequence (m::ms)) =
        if isCompound m then
            "(\{show m}) -> \{show (Sequence ms)}"
        else
            "\{show m} -> \{show (Sequence ms)}"

    show (Any []) = ""
    show (Any [m]) = show m
    show (Any (m::ms)) =
        if isCompound m then
            "(\{show m}) | \{show (Any ms)}"
        else
            "\{show m} | \{show (Any ms)}" 

    show (Transparent Nothing) = "Transparent"
    show (Transparent model) = "\{show model} -> Transparent"


Show ElementContentModel where
    show (>>> model) = show model
    show (When ms m) =
        let list = map (\(condition,model) => if isCompound model then
                "if \{show condition} then (\{show model})"
                else
                "if \{show condition} then \{show model}"
                ) ms
        in
        
        "\{joinBy "; " list}; else \{show m}"

Show (Map Attribute String) where
  show [] = ""
  show [(key,value)] = "\{show key}=\"{value}\""
  show ((key,value)::rest) = "\{show key}=\"{value}\" " ++ show rest 

Show Error where
    show (ViolatedRestriction error) = show error
    show (InvalidAttribute attr) = "Unknown attribute \"\{show attr}\""
    show (FailedContentModel contentModel) = "Invalid content. Expecting: \{show contentModel}"
 
Show Node where
    show (Branch element _) = "<\{show element.tag}>"
    show (Leaf (Text _)) = "text"
    show (Leaf (Comment _)) = "comment"

public export
Show (List (Node, Indexed Error)) where
    show =
        map(\(node, (line,col), error) =>
            "Ln \{show (line + 1)}, Col \{show (col + 1)}: \{show node} | \{show error}"
        )
        .> join "\n"

public export
[Full] Show Node where
    show (Leaf (Text text)) = text
    show (Leaf (Comment text)) = "<!--\{text}-->"
    show (Branch (Element tag []) children) = 
        "<\{show tag}>\{children |> map (assert_total show) |> join ""}</\{show tag}>"
    show (Branch (Element tag attributes) children) = 
        "<\{show tag} \{attributes |> Map.toList |> map (\(attr,val) => "\{show attr}=\"\{val}\"") |> join " "}>\{children |> map (assert_total show) |> join ""}</\{show tag}>"