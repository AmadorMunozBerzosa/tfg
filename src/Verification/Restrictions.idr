||| Tools to verify that a HTML follows the non-trivial restrictions associated with it
module Verification.Restrictions

import Util.Application
import Core.Node
import Core.Specification
import Core.Condition
import Data.String
import Data.Maybe
import Util.List
import Util.Map
import Util.Tree
import Verification.Attributes
import Verification.Aux
import Definitions.Specification
import Core.Tags
import Core.Attributes
import Util.Parser

public export
data ConditionError =
    -- Combinators
      (==>) Condition ConditionError
    | (||) Condition Condition
    -- General
    | ExpectedTag Tag
    | ExpectedCategory Category
    | NoContent
    | HasChildren
    | InvalidTagName String
    -- Attribute presence
    | ShouldHave Attribute
    | ShouldNotHave Attribute
    | ShouldHaveAny (List Attribute)
    | HasMultiple (List Attribute)
    -- Attribute values
    | Empty Attribute
    | IsNot Attribute Format String
    | Is Attribute String
    | Is' Attribute Format String
    | DoesNotIncludeAny Attribute (List String)
    | Includes Attribute String
    | Different Attribute Attribute
    | (>) Attribute Attribute
    | (<) Attribute Attribute
    -- Attributes values within the context of the document
    | NotUnique Attribute String (List NodePosition)
    | NotUniqueTag Attribute String (Maybe Tag) (List NodePosition)
    | NoReference Attribute Attribute String
    | NoReferenceTag Attribute (Tag, Attribute) String
    | NoReferenceAttribute Attribute (Attribute, Attribute) String
    | NoReferenceCategory Attribute (Category, Attribute) String
    | NotEachReferencesId Attribute (List String)
    | NotEachReferencesTh Attribute (List String)
    -- Parent element
    | WrongParent Tag Tag
    | ShouldNotHaveParent Tag
    -- Next sibling
    | NotNextSiblingAuto
    | NextSiblingIsNotAny (List Tag)
    -- Ancestor elements (parent, parent of parent, and so forth)
    | NoAncestor Tag
    | HasAncestor Tag (List NodePosition)
    | NoAncestorAttribute Tag Attribute
    -- Child elements
    | NoChild Tag
    | HasChildrenTag Tag (List NodePosition)
    | NoChildCategory Category
    | MultipleChild Tag (List NodePosition)
    | MultipleDefaultSubtitles (List NodePosition)
    | MultipleDefaultDescription (List NodePosition)

    -- Descendant elements (children, children of children, and so forth)
    | NoDescendant Tag
    | NoDescendantCategory Category
    | HasDescendant Tag (List NodePosition)
    | HasIndirectDescendant Tag (List NodePosition)
    | HasDescendantCategory Category (List NodePosition)
    | HasDescendantAttribute Attribute (List NodePosition)
    | MultipleDescendantCategory Category (List NodePosition)
    | MultipleDescendantAttribute Tag Attribute (List NodePosition)
    -- Special cases
    | URLsBeforeBase (List NodePosition)
    | MultipleCharset (List NodePosition)
    | MultipleTranslationPerLanguage
    | MultipleDescription
    | MultipleThemeColorPerMedia
    | MultipleColorScheme
    | MultipleEncoding
    | MultipleMetaState
    | IncorrectHeadingLevel
    | MultipleTrackPerLangAndLabel
    | IncorrectTableModel
    | HierarchicallyIncorrectMain (List NodePosition)
    | NoOtherRadioGroupOptions
    | DoesNotHavePlaceholderLabelOption
    | MultipleOpenPerGroup (List NodePosition)
    | NestedSameName (List NodePosition)

%ambiguity_depth 4

-- We need this mutual block because we need to know an element's cat to verify
-- conditions on it, but we must be able to verify a condition in order to extract an
-- element's actual categories
mutual
    ||| Realizes a potentially conditional cat into an actual cat if 
    ||| the element verifies the condition, or `Nothing` if it doesn't
    public export
    effectiveCategory: NodePosition -> ElementCategory -> Maybe Category
    effectiveCategory node (condition ==> cat) =
        if meets condition node then Just cat else Nothing
    effectiveCategory _ (>>> other) = Just other

    ||| List of categories an HTML element belongs to
    public export
    categories: NodePosition -> List Category
    categories node =
        case focus node of
            Leaf (Text string) =>
                if string |> trim |> null then
                    [Phrasing, Flow]
                else
                    [Phrasing, Flow, Palpable]
            
            Leaf (Comment _) => []
            
            Branch (Element tag _) _ =>
                mapMaybe (effectiveCategory node) (categories (specification tag))

    ||| Returns True when the currently focused element has the given category
    public export
    is: NodePosition -> Category -> Bool
    is node cat = categories node `contains` cat

    ||| Returns true if the given node errors the given condition
    public export
    meets: Condition -> NodePosition -> Bool
    meets condition node = null (errors condition node)

    ||| Returns a list of errors that are found when checking a condition
    ||| Most conditions returns either zero errors (if the condition is met)
    ||| or one error (if it isn't), but some can return multiple
    ||| (for example, conjunctions)
    public export
    errors: Condition -> NodePosition -> List ConditionError
    errors True _  = []
    errors (c && c') node = errors c node ++ errors c' node
    errors (c || c') node = if meets c node || meets c' node then [] else [c || c']

    errors (c ==> c') node =
        if meets c node then
            errors c' node |> map (\err => c ==> err)
        else
            []
    
    -- General conditions
    errors (Tag tag) node = if node `is` tag then [] else [ExpectedTag tag]
    errors (Category cat) node = if node `is` cat then [] else [ExpectedCategory cat]
    errors HasContent node =
        case children node |> find (focus .> hasContent) of
            Nothing => [NoContent]
            Just _ => []
    errors Childless node = if children node |> all (tag .> isNothing) then [] else [HasChildren]
    errors ValidTagName node =
        case tag node of
            Just (Custom name) =>
                if ("-" `isInfixOf` name) && not (("-" `isPrefixOf` name) || ("-" `isSuffixOf` name)) then
                    []
                else
                    [InvalidTagName name]
            _ => []

    -- Attribute presence
    errors (Has attr) node = if node `has` attr then [] else [ShouldHave attr]
    errors (HasNot attr) node = if node `has` attr then [ShouldNotHave attr] else []
    errors (HasAny attrs) node =
        case attrs |> filter (node `has`) of
            [] => [ShouldHaveAny attrs]
            _ => []
    errors (HasNone attrs) node =
        case attrs |> filter (node `has`) of
            [] => []
            attrs => map ShouldNotHave attrs
    errors (HasAtMostOne attrs) node =
        case attrs |> filter (node `has`) of
            [] => []
            [_] => []
            attrs' => [HasMultiple attrs']
    
    -- Attribute values
    errors (NotEmpty attr) node =
        case node !! attr of
            Nothing => []
            Just "" => [Empty attr]
            Just _ => []

    errors (Is attr format) node =
        case node !! attr of
            Nothing => []
            Just value => if matches attr format value then [] else [IsNot attr format value]

    errors (IsRequired attr format) node =
        case node !! attr of
            Nothing => [ShouldHave attr]
            Just value => if matches attr format value then [] else [IsNot attr format value]

    errors (IsNot attr string) node =
        case node !! attr of
            Nothing => []
            Just value => if toLower value == toLower string then [Is attr value] else []

    errors (IsRequiredNot attr format) node =
        case node !! attr of
            Nothing => [ShouldHave attr]
            Just value => if matches attr format value then [Is' attr format value] else []

    errors (IncludesAny attr values) node =
        case node !! attr of
            Nothing => []
            Just value =>
                if words value |> map toLower |> any ((map toLower values)  `contains`) then
                    []
                else
                    [DoesNotIncludeAny attr values]

    errors (IncludesRequired attr values) node =
        case node !! attr of
            Nothing => [ShouldHave attr]
            Just value =>
                if words value |> map toLower |> any ((map toLower values) `contains`) then
                    []
                else
                    [DoesNotIncludeAny attr values]

    errors (IncludesNone attr values) node =
        case node !! attr of
            Nothing => []
            Just value =>
                case words value |> map toLower |> find ((map toLower values) `contains`) of
                    Just value => [Includes attr value]
                    Nothing => []

    errors (Same attr attr') node =
        if map toLower (node !! attr) == map toLower (node !! attr') then
            []
        else
            [Different attr attr']
    
    errors (atr `Condition.(<=)` atr') node =
        let val = \atr => (node !! atr) >>= parse double in
        
        case [| val atr <= val atr' |] of
            Just False => [atr > atr']
            _ => []



    errors (atr `Condition.(>=)` atr') node =
        let val = \atr => (node !! atr) >>= parse double in
        case [| val atr >= val atr' |] of
            Just False => [atr < atr']
            _ => []

    errors (Unique attr) node =
        case node !! attr of
            Nothing => []
            Just value =>
                let sameValue = root node
                                |> descendants
                                |> filter (\node' => node' !! attr == Just value)
                in

                if length sameValue > 1 then
                    [NotUnique attr value sameValue]
                else
                    []

    errors (UniqueTag attr) node =
        case node !! attr of
            Nothing => []
            Just value =>
                let sameValue = root node
                                |> descendants
                                |> filter (\node' => tag node' == tag node && node' !! attr == Just value)
                in

                if length sameValue > 1 then
                    [NotUniqueTag attr value (tag node) sameValue]
                else
                    []

    -- Attributes values within the context of the document
    errors (References attr attr') node =
        case node !! attr of
            Just value =>
                let reference =
                    root node |> findElement (\node => node !! attr' == Just value)
                in

                if isJust reference then [] else [NoReference attr attr' value]

            Nothing => []
    
    errors (ReferencesTag attr (tag',attr')) node =
        case node !! attr of
            Just value =>
                let reference =
                        root node
                        |> findElement (\node =>
                            tag node == Just tag'
                            && node !! attr' == Just value
                            )
                in

                if isJust reference then [] else [NoReferenceTag attr (tag', attr') value]

            Nothing => []
    
    errors (ReferencesAttribute attr (attr'',attr')) node =
        case node !! attr of
            Just value =>
                let reference =
                        root node
                        |> findElement (\node =>
                            node !! attr' == Just value
                            && (node `has` attr'')
                        )
                in

                if isJust reference then [] else [NoReferenceAttribute attr (attr'',attr') value]

            Nothing => []
    
    errors (ReferencesCategory attr (cat,attr')) node =
        case node !! attr of
            Just value =>
                let reference =
                        root node
                        |> findElement (\node =>
                            (node `is` cat)
                            && node !! attr' == Just value
                        )
                in

                if isJust reference then [] else [NoReferenceCategory attr (cat,attr') value]
            Nothing => []

    errors (EachReferencesId attr) node =
        case node !! attr of
            Just value =>
                let invalid = 
                        words value
                        |> filter (\value =>
                                root node
                                |> findElement (\node => node !! Id == Just value)
                                |> isNothing
                            )
                in

                if null invalid then [] else [NotEachReferencesId attr invalid]

            Nothing => []
                
    errors (EachReferencesTh attr) node =
        let ids = node !! attr
            table = closest (`is` Table) node in

        case (ids, table) of
            (Just value, Just table) =>
                let invalid =
                        words value
                        |> filter (\value =>
                                table
                                |> findElement (\node => node !! Id == Just value && (node `is` Th))
                                |> isNothing
                            )
                in

                if null invalid then [] else [NotEachReferencesId attr invalid]
            _ => []
        
    -- Parent element

    errors (HasParent tag') node =
        case parent node >>= tag of
            Just tag => if tag == tag' then [] else [WrongParent tag tag']
            Nothing => []

    errors (NotParent tag') node =
        case parent node >>= tag of
            Just tag => if tag == tag' then [ShouldNotHaveParent tag'] else []
            Nothing => []
    
    -- Next sibling
    errors NextSiblingAuto node =
        case next node of
            Just next =>
                if (
                    (next `is` Img)
                    &&
                    (next !! Loading == Just "lazy")
                    &&
                    (next !! Sizes == Just "auto"
                    || ("auto," `isPrefixOf` (next !! Sizes |> fromMaybe "")))
                ) then [] else [NotNextSiblingAuto]
                
            Nothing => [NotNextSiblingAuto]

    errors (NextSiblingTag tags) node =
        case next node of
            Just next => if any (next `is`) tags then [] else [NextSiblingIsNotAny tags]
            Nothing => [NextSiblingIsNotAny tags]

    -- Ancestor elements
    errors (HasAncestor tag) node =
        if ancestors node |> any (`is` tag) then
            []
        else
            [NoAncestor tag]

    errors (NoAncestor tag) node =
        case ancestors node |> filter (`is` tag) of
            [] => []
            nodes => [HasAncestor tag nodes]

    errors (HasAncestorAttribute tag attr) node =
        if ancestors node |> any (\node => (node `is` tag) && (node `has` attr)) then
            []
        else
            [NoAncestorAttribute tag attr]
    
    -- Child elements
    errors (HasChild tag) node =
        if children node |> any (`is` tag) then
            []
        else [NoChild tag]

    errors (NoChild tag) node =
        case children node |> filter (`is` tag) of
            [] => []
            nodes => [HasChildrenTag tag nodes]

    errors (HasChildCategory cat) node =
        if children node |> any (`is` cat) then
            []
        else
            [NoChildCategory cat]

    errors (UniqueChild tag) node =
        case children node |> filter (`is` tag) of
            [] => []
            [_] => []
            nodes => [MultipleChild tag nodes]

    errors UniqueDefaultSubtitles node =
        case children node
        |> filter (\child =>
            (child `is` Track)
            && (child `has` Default)
            && ([Just "captions", Just "subtitles"] `contains` (child !! Kind))
        ) of
            [] => []
            [_] => []
            nodes => [MultipleDefaultSubtitles nodes]

    errors UniqueDefaultDescription node =
        case children node
        |> filter (\child =>
            (child `is` Track)
            && (child `has` Default)
            && (Just "descriptions" == (child !! Kind))
        ) of
            [] => []
            [_] => []
            nodes => [MultipleDefaultDescription nodes]

    -- Descendant elements
    errors (HasDescendant tag) node =
        if descendants node |> any (`is` tag) then
            []
        else
            [NoDescendant tag]

    errors (HasDescendantCategory cat) node =
        if descendants node |> any (`is` cat) then
            []
        else
            [NoDescendantCategory cat]

    errors (NoDescendant tag) node =
        case descendants node |> filter (`is` tag) of
            [] => []
            nodes => [HasDescendant tag nodes]

    errors (NoIndirectDescendant tag) node =
        case children node
        >>= (\child => descendants child |> filter (`is` tag)) of
            [] => []
            nodes => [HasIndirectDescendant tag nodes]

    errors (NoDescendantCategory cat) node =
        case descendants node |> filter (`is` cat) of
            [] => []
            nodes => [HasDescendantCategory cat nodes]

    errors (NoDescendantAttribute attr) node =
        case descendants node |> filter (`has` attr) of
            [] => []
            nodes => [HasDescendantAttribute attr nodes]

    errors (UniqueDescendantCategory cat) node =
        case descendants node |> filter (`is` cat) of
            [] => []
            [_] => []
            nodes => [MultipleDescendantCategory cat nodes]
    errors (UniqueDescendantAttribute tag attr) node =
        case descendants node |> filter (\node' => (node' `is` tag) && (node' `has` attr)) of
            [] => []
            [_] => []
            nodes => [MultipleDescendantAttribute tag attr nodes]

    -- Special cases

    errors BeforeURLs node =
        case node |> allPrevious |> filter hasURL of
            [] => []
            nodes => [URLsBeforeBase nodes]

    errors UniqueCharset head =
        case head |> descendants |> filter (`is` Meta) |> filter (`has` Charset) of
            [] => []
            [_] => []
            nodes => [MultipleCharset nodes]

    errors UniqueTranslationPerLanguage head =
        let langs =
                descendants head
                |> filter (`is` Meta) |> filter (metaHasName "application-name")
                |> map metaLang
        in
        
        if isSet langs then
            []
        else
            [MultipleTranslationPerLanguage] 

    errors UniqueDescription head =
        let withDescription =
                descendants head
                |> filter (`is` Meta)
                |> filter (metaHasName "description")
        in
        
        if length withDescription > 1 then
            [MultipleDescription]
        else
            []

    errors UniqueThemeColorPerMedia head =
        let media =
                descendants head
                |> filter (`is` Meta) |> filter (metaHasName "theme-color")
                |> map metaMedia
        in
                
        if isSet media then
            []
        else
            [MultipleThemeColorPerMedia]

    errors UniqueColorScheme head =
        let withColorScheme =
                descendants head
                |> filter (`is` Meta)
                |> filter (metaHasName "color-scheme")
        in

        if length withColorScheme > 1 then
            [MultipleColorScheme]
        else
            []

    errors UniqueEncoding head =
        let elems = descendants head |> filter (`is` Meta)
            withContentType = elems |> filter (metaHasState "content-type")
            withCharset = elems |> filter (metaHasName "charset")
        in

        case withContentType ++ withCharset of
            [] => []
            [_] => []
            _ => [MultipleEncoding]

    errors UniqueMetaState head =
        if head |> descendants |> filter (`is` Meta) |> mapMaybe metaState |> isSet then
            []
        else 
            [MultipleMetaState]

    errors UniqueTrackPerLangAndLabel media =
        let tracks = children media |> filter (`is` Track) in

        if tracks |> map (\track => (track !! Kind, track !! Srclang, track !! Label)) |> isSet then
            []
        else
            [MultipleTrackPerLangAndLabel]

    errors CorrectTableModel node = if validTable node then [] else [IncorrectTableModel]
    
    errors HierarchicallyCorrectMain node =
        let forbidden = \node => case tag node of
                Just Html => False
                Just Body => False
                Just Div => False
                Just Form => False
                Just (Custom _) => False
                _ => True in
        
        case ancestors node |> filter forbidden of
            [] => []
            nodes => [HierarchicallyIncorrectMain nodes]

    errors CorrectHeadingLevel node =
        case outline node of
            [] => []
            list =>
                if (list `contains` 1) && validOutline list then
                    []
                else
                    [IncorrectHeadingLevel]
    
    errors OtherRadioGroupOptions node =
        let
            name = node !! Name
            for = node !! For
            form = closest (`is` Form) node
            in

        case (name, form, for) of
            (Just name, Just form, _) =>
                let group =
                    form |> findElements (\node =>
                        (node `is` Input)
                        && (node !! Type') == Just "radio"
                        && (node !! Name) == Just name
                        )
                in

                if length group > 1 then
                    []
                else
                    [NoOtherRadioGroupOptions]

            (Just name, _, Just for) =>
                let group =
                    root node |> findElements (\node =>
                        (node `is` Input)
                        && (node !! Name) == Just name
                        && (node !! For) == Just for
                        )
                in

                if length group > 1 then
                    []
                else
                    [NoOtherRadioGroupOptions]
            (_, _) => []

    errors HasPlaceholderLabelOption node =
        if children node
        |> any (\node =>
            (node `is` Option) && (
                case node !! Value of
                    Nothing => all (focus .> hasContent .> not) (children node)
                    Just "" => True
                    Just _ => False
            )
        ) then
            []
        else
            [DoesNotHavePlaceholderLabelOption]
            
    errors UniqueOpenPerGroup node =
        case node !! Name of
            Just name =>
                case root node
                |> findElements (\node =>
                    (node `is` Details)
                    && (node `has` Open)
                    && ((node !! Name) == Just name)
                )
                of
                    [] => []
                    [_] => []
                    nodes => [MultipleOpenPerGroup nodes]
            Nothing => []

    errors NotNestedSameName node =
        case (node !! Name) of
            Just name =>
                case node |> ancestors |> filter (\node' => node' !! Name == Just name) of
                    [] => []
                    nodes => [NestedSameName nodes]
            _ => []

||| Conditions that apply to all HTML elements
public export
globalRestrictions : Condition
globalRestrictions = [
    Lang `Is` LanguageCode,
    Class `Is` Set " " Anything,
    Id `Is` NoWhitespace,
    Id `IsNot` "",
    Unique Id,
    Translate `Is` "yes" || "no",
    Dir `Is` "ltr" || "rtl" || "auto"
]

||| Returns the list of restrictions a node doesn't meet
public export
validate: NodePosition -> List ConditionError
validate node =
    case element node of
        Just (Element tag _) =>
            errors (globalRestrictions :: (specification tag).restrictions) node     
        Nothing => []