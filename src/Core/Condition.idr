||| Collection of types associated with conditions on HTML elements
module Core.Condition

import Derive.Prelude
import Core.Tags
import Core.Attributes
import Data.String

%language ElabReflection

namespace Category
    ||| Kind of element according to its role within its containing document
    ||| See: https://html.spec.whatwg.org/multipage/dom.html#content-categories
    public export
    data Category =
          Metadata
        | Flow
        | Sectioning
        | Heading
        | Phrasing
        | Embedded
        | Interactive
        | Palpable
        | ScriptSupporting
        | FormAssociated
        | Listed
        | SelectElementInnerContent
        | OptgroupElementInnerContent
        | OptionElementInnerContent
        | Labelable
        | Submitable
        | Resettable
        | AutocapitalizeAndAutocorrectInheriting

%runElab derive "Category" [Eq]

public export prefix 10 >>>


||| A condition that a HTML node can meet (or not) within a HTML tree
||| It is used both for defining the specification for an element
||| and for reporting invalid elements granularly
public export
data Condition =
    -- Combinators
      True -- Always True
    | (&&) Condition Condition -- Conjunction
    | (==>) Condition Condition -- Implication
    | (||) Condition Condition -- Disjunction

    -- General conditions
    | Tag Tag -- The node is an element with a specific tag
    | Category Category -- The node belongs to a specific category
    | HasContent -- The node is not empty
    | Childless -- The node is an element and has no children
    | ValidTagName -- The element's tag is valid as a custom element name

    -- ========================
    -- Conditions on attributes
    -- ========================

    -- Attribute presence
    | Has Attribute -- The node has an attribute
    | HasNot Attribute -- The node doesn't have an attribute
    | HasAny (List Attribute) -- The node has at least one of a list of attributes
    | HasNone (List Attribute) -- The node doesn't have any of a list of attributes
    | HasAtMostOne (List Attribute) -- The node has at most one of a list of attributes

    -- Attribute values
    | NotEmpty Attribute -- An attribute doesn't have the empty value, or is absent
    | Is Attribute Format -- An attribute value follows a format, or it is absent
    | IsRequired Attribute Format -- An attribute value is present and follows a format
    | IsNot Attribute String -- An attribute doesn't have an specific value, or it is absent
    | IsRequiredNot Attribute Format -- An attribute value is present and doesn't follow a format
    | IncludesAny Attribute (List String) -- An attribute value contains one of the given values, or it is absent
    | IncludesRequired Attribute (List String) -- An attribute value is present and contains one of the given values
    | IncludesNone Attribute (List String) -- An attribute value contains none of the given values, or it is absent
    | Same Attribute Attribute -- The attributes have the same value, or are both absent
    | (<=) Attribute Attribute -- attribute1 <= attribute2, or one value is absent
    | (>=) Attribute Attribute -- attribute1 >= attribute2, or one value is absent

    -- Attributes values within the context of the document
    | Unique Attribute -- The value of the attribute is unique within the document
    | UniqueTag Attribute  -- The value of the attribute is unique among the elements with the same tag
    | References Attribute Attribute
        -- The attribute value references the attribute value of another element
    | ReferencesTag Attribute (Tag, Attribute)
        -- Same as References, but the referenced element must have a specific tag
    | ReferencesAttribute Attribute (Attribute, Attribute)
        -- Same as References, but the referenced element must have a specific attribute
    | ReferencesCategory Attribute (Category, Attribute)
        -- Same as References, but the referenced element must belong to a specific category
    | EachReferencesId Attribute
        -- The attribute value is a list of values, each of which references an ID of an element
    | EachReferencesTh Attribute
        -- The attribute value is a list of values, each of which references the ID of a <th> element
        -- within the same table
    
    -- ===================================
    -- Conditions on related HTML elements
    -- ===================================

    -- Parent element
    | HasParent Tag -- Has a specific tag as its parent
    | NotParent Tag -- Doesn't have a specific tag as its parent

    -- Next sibling (i.e. the following element)
    | NextSiblingAuto -- Next element is an image that allows auto sizing
    | NextSiblingTag (List Tag) -- Next element is one out of a list of tags

    -- Ancestor elements (parent, parent of parent, and so forth)
    | HasAncestor Tag -- Has an ancestor with the given tag
    | NoAncestor Tag -- Doesn't have an ancestor with the given tag
    | HasAncestorAttribute Tag Attribute -- Has an ancestor with the given tag and the given attribute

    -- Child elements
    | HasChild Tag -- Has a child with the given tag
    | NoChild Tag -- Doesn't have a child with the given tag
    | HasChildCategory Category -- Has a child with the given vategory
    | UniqueChild Tag -- Has only one child with the given tag
    | UniqueDefaultSubtitles -- Has only one <track> subtitles or captions child with the "default" attribute
    | UniqueDefaultDescription -- Has only one <track> descriptions child with the "default" attribute

    -- Descendant elements (children, children of children, and so forth)
    | HasDescendant Tag -- Has a descendant with the given tag
    | HasDescendantCategory Category -- Has a descendant in the given category
    | NoDescendant Tag -- Doesn't have a descendant with the given tag
    | NoIndirectDescendant Tag -- The node's children can not have a descendant with the given tag
    | NoDescendantCategory Category -- Doesn't have a descendant in the given category
    | NoDescendantAttribute Attribute -- Doesn't have a descendant with the given attribute
    | UniqueDescendantCategory Category -- Has only one descendant in the given category
    | UniqueDescendantAttribute Tag Attribute -- Has only one descendant with the given tag and attribute
    
    -- Special cases

    | BeforeURLs -- The element, if present, appears before any URL attribute
    | UniqueCharset -- The document's charset is declared at most once
    | UniqueTranslationPerLanguage  -- There is only one translation declared for each declared language
    | UniqueDescription -- The document's description is declared at most once
    | UniqueThemeColorPerMedia -- The document's theme color is declared at most once per each declared media query
    | UniqueColorScheme -- The document's color scheme is declared at most once
    | UniqueEncoding -- The document's encoding is declared at most once
    | UniqueMetaState -- The document's HTTP-Equiv values within <meta> elements don't repeat
    | CorrectHeadingLevel -- Headings are nested correctly, according to the heading model
    | UniqueTrackPerLangAndLabel -- There is only one <track> element per language and label in the media element
    | CorrectTableModel -- Table cells nest without overlap
    | HierarchicallyCorrectMain -- The main element is correct within its hierarchy
    | OtherRadioGroupOptions -- A radio group consists of more than one radio button
    | HasPlaceholderLabelOption -- Within a <select> element, one option is a valid default option
    | UniqueOpenPerGroup -- For a group of <details>, at most one has the "open" attribute
    | NotNestedSameName -- Nested <details> elements don't share the same name
    
public export infixr 2 <==>, ==>
public export infixr 3 `Is`, `Includes`, `IsNot`, `IsRequired`,
    `IncludesAny`, `IncludesNone`, `IncludesRequired`, `IsRequiredNot`

-- We allow ourselves to write conditions as lists, interpretating them as conjunctions
public export
Nil: Condition 
Nil = True

public export
(::): Condition -> Condition -> Condition
(::) = (&&)


-- Auxiliary operator definitions. Defined for syntactic convenience
namespace Attribute
    public export
    (<=) : Attribute -> Double -> Condition
    attr <= double = attr `Is` LessThan double

    public export
    (>=) : Attribute -> Double -> Condition
    attr >= double = attr `Is` GreaterThan double

-- More auxiliary operator definitions. Defined for syntactic convenience
namespace Attribute'
    public export
    (<=) : Double -> Attribute -> Condition
    (<=) = flip (<=)

    public export
    (>=) : Double -> Attribute -> Condition
    (>=) = flip (>=)