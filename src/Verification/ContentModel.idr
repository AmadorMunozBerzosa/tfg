||| Tools to verify that a HTML element's children follow its parent's content model
module Verification.ContentModel 

import Util.Application
import Core.Tags
import Core.Node
import Core.Specification
import Data.String
import Util.List
import Util.Map
import Util.Tree
import Util.Parser
import Verification.Restrictions
import Definitions.Specification
import Data.List.Elem
import Core.Condition

||| Realizes a potentially conditional content model into an actual content model
||| by checking the condition that meets
public export
effectiveContentModel: NodePosition -> ElementContentModel -> ContentModel
effectiveContentModel node (>>> model) = model
effectiveContentModel node (When [] else') = else'
effectiveContentModel node (When ((condition,model)::xs) else') =
    if meets condition node then
        model
    else
        effectiveContentModel node (When xs else')

||| Given a HTML node, it return the content model it follows
||| if it's a element node, or `Nothing` otherwise
public export
contentModel : NodePosition -> Maybe ContentModel
contentModel node =
    case tag node of
        Nothing => Nothing
        Just tag => Just (specification tag |> contentModel |> effectiveContentModel node)

||| Returns a parser that checks whether a list of children follows the given model
public export
parser': ContentModel -> Parser (List NodePosition) ()

parser' Anything = anything

parser' Nothing = all (\node =>
    case focus node of
        Leaf (Text "") => True
        Leaf (Comment _) => True
        _ => False
    )

parser' Text =
    atomic (\node =>
        case focus node of
            Leaf (Text _) => True
            _ => False
    )
    |> many
    |> ignore

parser' (Tag t) = atomic (\node => tag node == Just t)
parser' (Category category) = atomic (\node => category `elem` categories node)

parser' (Optional model) = parser' model |> optional |> ignore
parser' (Many model) = parser' model |> many |> ignore
parser' (AtLeastOne model) = parser' model |> atLeastOne |> ignore
parser' (Intermixed category model) = parser' model |> intermixed (\node => category `elem` categories node)
parser' (Sequence list) = sequence_ (map parser' list)
parser' (Any list) = choice (map (parser' .> delay) list)

parser' (Transparent _) = anything -- Transparent elements delegate their requirements to their parent

||| Replaces transparent elements with their children in a HTML tree
||| If they require some elements before becoming transparent,
||| those are not included in the replacement
public export
replaceTransparent: List NodePosition -> List NodePosition
replaceTransparent nodes =
    nodes >>= (\node =>
        case contentModel node of
            Just (Transparent Nothing) => replaceTransparent (children node)
            Just (Transparent model) =>
                case run (parser' model) (children node) of
                    Reject => nodes
                    Accept () remaining => replaceTransparent remaining
            _ => [node]
    )

||| Removes comments and text nodes containing only whitespace characters from a list of nodes
public export
removeWhitespaceAndComments: List NodePosition -> List NodePosition
removeWhitespaceAndComments = filter (\node =>
    case focus node of
        Leaf (Comment _) => False
        Leaf (Text string) => string |> trim |> null |> not
        _ => True
    )

||| Returns a parser that checks whether a list of children follows the given model.
||| It first removes whitespace text nodes and comments, and it also accounts for transparent elements
public export
parser: ContentModel -> Parser (List NodePosition) ()
parser model = From (\elements =>
    run (parser' model) ((removeWhitespaceAndComments .> replaceTransparent) elements)
    )
    

public export
validate: NodePosition -> Maybe ContentModel
validate node =
    case contentModel node of
        Just model =>
            if parses (parser model) (children node) then
                Nothing
            else
                Just model
        Nothing => Nothing
