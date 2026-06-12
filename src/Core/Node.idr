||| Implementation for HTML documents as Trees of HTML Nodes
module Core.Node

import Core.Tags
import Core.Attributes
import Util.List
import Util.Tree
import Util.Map
import Util.Application
import Data.String
import Data.List


||| Character Data: plain text or a comment
public export
data CData = Text String | Comment String

||| HTML element node
public export
record ElementNode where
    constructor Element
    tag: Tag
    attributes: Map Attribute String

||| A HTMLNode can be text, a comment or an element
||| Other kinds of nodes are admitted in HTML-like
||| XML documents, but are technically excluded from the spec
public export
Node: Type
Node = Tree CData ElementNode

||| Alias for the Zipper of `Node` values
public export
NodePosition: Type
NodePosition = Zipper CData ElementNode

||| Tag name for an element node, or Nothing for any other kind of node
public export
tag: Node -> Maybe Tag
tag node =
  case node of
    Branch (Element tag _) _ => Just tag
    _ => Nothing

||| Returns True if the node is an element with the given tag
public export
is: Node -> Tag -> Bool
is node tagName = tag node == Just tagName

||| Returns True if the node contains non-whitespace content
public export
hasContent: Node -> Bool
hasContent (Leaf (Comment _)) = False
hasContent (Leaf (Text string)) = (trim .> null .> not) string
hasContent _ = True

||| Returns the node's attributes, or the empty map if it's not an element
public export
attributes: Node -> Map Attribute String
attributes (Leaf _) = []
attributes (Branch (Element _ attrs) _) = attrs

namespace Tree
  ||| Returns the currently focused element, or Nothing if the focus is on a leave
  public export
  element: NodePosition -> Maybe ElementNode
  element node =
    case focus node of
      Leaf _ => Nothing
      Branch element _ => Just element

  ||| Returns the currently focused element's attributes, or Nothing if the focus is on a leave
  public export
  attributes: NodePosition -> Map Attribute String
  attributes node = attributes (focus node)

  ||| Returns the attribute value of the currently focused element's attributes,
  ||| or Nothing if the focus is on a leave
  public export
  (!!): NodePosition -> Attribute -> Maybe String
  (!!) node attr = lookup attr (attributes (focus node))

  ||| Returns True if the currently focused node has the attribute
  public export
  has: NodePosition -> Attribute -> Bool
  has node attr = attributes (focus node) `has` attr
  
  ||| Returns the currently focused element's tag, or Nothing if the focus is on a leave
  public export
  tag: NodePosition -> Maybe Tag
  tag node = map (.tag) (element node)

  ||| Returns True when the currently focused element is an element with the given tag
  public export
  is: NodePosition -> Tag -> Bool
  is node t = tag node == Just t

  ||| Finds the element among a tree's descendants that meets the predicate
  ||| and that has the given value for the given attribute
  public export
  findElementBy: Attribute -> String -> (NodePosition -> Bool) -> NodePosition -> Maybe NodePosition
  findElementBy attr value condition node =
    descendants node
    |> filter condition
    |> find (\node' => node' !! attr == Just value)

  ||| Finds the element among a tree's descendants that meets the predicate
  ||| and that has the given value for the given attribute
  public export
  findElement: Attribute -> String -> NodePosition -> Maybe NodePosition
  findElement attr value node =
    findElementBy attr value (const True) node

  ||| Finds all the elements among a tree's descendants that meets the predicate
  ||| and that has the given value for the given attribute
  public export
  findElementsBy: Attribute -> String -> (NodePosition -> Bool) -> NodePosition -> List NodePosition
  findElementsBy attr value condition node =
    descendants node
    |> filter (\node' => condition node' && (node' !! attr) == Just value)