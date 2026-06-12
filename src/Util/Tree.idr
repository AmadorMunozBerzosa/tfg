||| General implementation of Rose-like Trees
||| It is used both for implementing HTML documents
||| and validation trees that reflect the structure
||| of the document they validate
module Util.Tree

import Util.List
import Data.String


||| N-ary tree with (potentially) different types
||| on its leafs and nodes
||| A branch with no children is considered valid
||| and different from a Leaf
public export
data Tree a b = Leaf a | Branch b (List (Tree a b))

namespace Tree
  ||| Shortand for trees with only one kind of value
  public export
  Tree: Type -> Type
  Tree a = Tree a a

||| Converts a Tree to a List, depth first
public export
toList: Tree a -> List a
toList (Leaf a) = [a]
toList (Branch a children) = a :: (children >>= assert_total toList)

public export
split: Tree (a,a') (b,b') -> (Tree a b, Tree a' b')
split (Leaf (a,a')) = (Leaf a, Leaf a')
split (Branch (b,b') children) =
  let (children, children') = split (map split children) in

  (Branch b children, Branch b' children')

public export
infixr 9 !!

||| Tries to obtain an element of a tree by repeatedly indexing into the nodes
||| Returns Nothing if there is no element with the given index
public export
(!!): Tree a -> List Nat -> Maybe a
(Leaf a) !! [] = Just a
(Leaf a) !! _ = Nothing
(Branch a _) !! [] = Just a
(Branch a children) !! (i::is) =
  case i `inBounds` children of
    Yes _ => (index i children) !! is
    No _ => Nothing

||| A description of the siblings within a specific level of a tree
public export
record Crumbs a b where
  constructor Crumb
  label: b
  before: List (Tree a b)
  after: List (Tree a b)

||| A snapshot of iterating a Tree.
||| In other words, a position within a tree
public export
record Zipper a b where
  constructor Zip
  focus: Tree a b
  before: List (Tree a b)
  after: List (Tree a b)
  crumbs: List (Crumbs a b)

||| Starts the iteration by placing the focus on the root
public export
fromTree: Tree a b -> Zipper a b
fromTree a = Zip a [] [] []

||| Moves the iterator to the left (i.e. to the previous sibling, if any)
public export
previous : Zipper a b -> Maybe (Zipper a b)
previous (Zip focus [] after path) = Nothing
previous (Zip focus (x::before) after path) = Just (Zip x before (focus::after) path)

||| Moves the iterator to the right (i.e. to the next sibling, if any)
public export
next : Zipper a b -> Maybe (Zipper a b)
next (Zip focus before [] path) = Nothing
next (Zip focus before (x::after) path) = Just (Zip x (focus::before) after path)

||| Moves the iterator upwards (i.e. to the parent, if any)
public export
parent : Zipper a b -> Maybe (Zipper a b)
parent (Zip _ _ _ []) = Nothing
parent (Zip focus before after (Crumb label before' after' :: xs)) =
  Just (
    Zip (Branch label (reverse before ++ [focus] ++ after)) before' after' xs
  )

||| Moves the iterator downwards (i.e. to the first child, if any)
public export
child : Zipper a b -> Maybe (Zipper a b)
child (Zip (Leaf _)  __ _) = Nothing
child (Zip (Branch _ []) _ _ _) = Nothing
child (Zip (Branch label (x::xs)) before after list) =
  Just (
    Zip x [] xs (Crumb label before after :: list)
  )

||| Moves the iterator to the top of the tree
public export
root: Zipper a b -> Zipper a b
root node =
  case parent node of
    Just parent => assert_total root parent
    Nothing => node

||| All siblings of a given node, from that point onwards, including itself
public export
nextSiblings: Zipper a b -> List (Zipper a b)
nextSiblings zipper =
  case next zipper of
    Just sibling => zipper :: assert_total nextSiblings sibling
    Nothing => [zipper]

||| All siblings of a given node, up to that point, including itself
public export
previousSiblings: Zipper a b -> List (Zipper a b)
previousSiblings zipper =
  case previous zipper of
    Just sibling => zipper :: assert_total previousSiblings sibling
    Nothing => [zipper]

||| Children nodes of a given node
||| Returns the empty list if it's a leaf (or a branch with no children)
public export
children: Zipper a b -> List (Zipper a b)
children node =
  case child node of
    Nothing => []
    Just child => nextSiblings child

||| Descendants of a given node (i.e. children, children of children and so forth)
||| Returns the empty list if it's a leaf (or a branch with no children)
public export
descendants: Zipper a b -> List (Zipper a b)
descendants node =
  let children = children node in
  children ++ (children >>= assert_total descendants)


||| The node path from the actual node to the root of the tree
public export
ancestors: Zipper a b -> List (Zipper a b)
ancestors zipper = reverse (go zipper) where
  go : Zipper a b -> List (Zipper a b)
  go zipper =
    case parent zipper of
      Just parent => parent :: assert_total ancestors parent
      Nothing => []

||| Closest in a node's list of ancestors that meets a predicate, if any
public export
closest : (Zipper a b -> Bool) -> Zipper a b -> Maybe (Zipper a b)
closest pred node = 
  if pred node then
    Just node
  else 
    parent node >>= assert_total closest pred

||| Gets all the elements that appear before a given element in the tree
public export
allPrevious: (Zipper a b) -> List (Zipper a b)
allPrevious zipper =
  let notInclusive: List (Zipper a b) -> List (Zipper a b)
      notInclusive [] = []
      notInclusive (_::rest) = reverse rest 
  in

  (notInclusive (previousSiblings zipper) >>= (\sibling => sibling :: reverse (descendants sibling)))
  ++
  (case parent zipper of
    Nothing => []
    Just zipper' => assert_total allPrevious zipper'
    )

||| Returns the list of indices from the currently focused node
||| to the root of the tree
public export
index: Zipper a b -> List Nat
index zipper =
  case parent zipper of
    Nothing => []
    Just zipper' => length (zipper.before) :: assert_total index zipper' 

||| Given a tree position and a different tree, it tries to access
||| the same position in the different tree
public export
parallel: Zipper b c -> Tree a -> Maybe a
parallel zipper tree = tree !! (reverse (index zipper))