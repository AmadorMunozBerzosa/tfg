||| Auxiliary definitions to verify conditions on nodes
module Verification.Aux

import Core.Node
import Core.Specification
import Core.Condition
import Data.String
import Data.Maybe
import Util.List
import Util.Map
import Util.Application
import Util.Tree
import Definitions.Specification
import Core.Tags
import Core.Attributes
import Util.Parser

||| List of restrictions an HTML element has
public export
restrictions: NodePosition -> Condition
restrictions node =
    case element node of
    Nothing => True
    Just (Element tag attributes) => (specification tag).restrictions

||| Returns the form tag that owns the given tag, if any
public export
owner: NodePosition -> Maybe NodePosition
owner node =
    case node !! For of
        Just form => root node |> findElement (\node => node !! Name == Just form)
        Nothing => closest (`is` Form) node

-- Functions for verifying that headings are well nested, according to:
-- https://html.spec.whatwg.org/multipage/sections.html#outline
namespace Heading
    ||| Returns the heading's offset, i.e. the modification that is applid
    ||| to its natural level
    public export
    offset : NodePosition -> Nat
    offset node =
        case element node of
            Nothing => 0
            Just (Element _ attributes) =>
                let nextOffset =
                    case lookup Headingoffset attributes >>= parsePositive of
                        Just num => num
                        Nothing => 0
                    in

                if attributes `has` Headingreset then nextOffset else
                
                case parent node of
                    Just parent => nextOffset + assert_total offset parent
                    Nothing => nextOffset

    ||| If the heading level of a node if it's a heading, or `Nothing` otherwise
    public export
    level : NodePosition -> Maybe Nat
    level node =
        let base =
            case tag node of
                Just H1 => Just 1
                Just H2 => Just 2
                Just H3 => Just 3
                Just H4 => Just 4
                Just H5 => Just 5
                Just H6 => Just 6
                _ => Nothing
        in

        base |> map (\base => min 9 (base + offset node))
        

    ||| Returns the list of heading levels found within the element
    public export
    outline : NodePosition -> List Nat
    outline = descendants .> mapMaybe level                

    ||| Returns True if an outline is valid
    public export
    validOutline: List Nat -> Bool
    validOutline [] = True
    validOutline [_] = True
    validOutline (lead::new::rest) = (new <= lead + 1) && validOutline (new::rest)

-- Auxiliary function for verifying <meta> elements
namespace Meta
    ||| Returns true if the element is a <meta> tag
    ||| and the "name" attribute has the given value
    public export
    metaHasName : String -> NodePosition -> Bool
    metaHasName name node = map toLower (node !! Name) == Just name

    ||| Returns true if the element is a <meta> tag
    ||| and the "http-equiv" attribute has the given value
    public export
    metaHasState : String -> NodePosition -> Bool
    metaHasState name node = map toLower (node !! HttpEquiv) == Just name

    ||| Returns the value of the "http-equiv" attribute
    ||| if present on a <meta> element, or `Nothing` otherwise
    public export
    metaState : NodePosition -> Maybe String
    metaState node = map toLower (node !! HttpEquiv)

    ||| Returns the value of the "lang" attribute if present on a <meta> element
    ||| with name="application-name", or `Nothing` otherwise
    public export
    metaLang : NodePosition -> Maybe String
    metaLang node = map toLower (node !! Lang) 

    ||| Returns the value of the "media" attribute if present on a <meta> element
    ||| with name="theme-color", or `Nothing` otherwise
    public export
    metaMedia : NodePosition -> Maybe String
    metaMedia node = map toLower (node !! Media) 

-- Functions for verifying that table model of a <table> tag is well defined
namespace Table
    ||| Given a <table> element, it returns the list of <tr> elements it contains
    ||| (either directly or inside a <thead>|<tbody>|<tfoot> element)
    public export
    rows: NodePosition -> List NodePosition
    rows table =
        children table >>= (
            \node =>
                case tag node of
                    Just Tr => [node]
                    Just Thead => node |> children |> filter (`is` Tr)
                    Just Tbody => node |> children |> filter (`is` Tr)
                    Just Tfoot => node |> children |> filter (`is` Tr)
                    _ => []
        )

    ||| Given a <tr> element, it returns the list of its <td>|<th> children
    public export
    cells: NodePosition -> List NodePosition
    cells = children .> filter (\node => (node `is` Td) || (node `is` Th))

    ||| Extracts the (rowspan,colspan) dimensions of a <td> element
    public export
    tdDimensions: NodePosition -> (Nat,Nat)
    tdDimensions td =
        let parse = \attr => (td !! attr) >>= parsePositive |> fromMaybe (the Nat 1) in
        
        (parse Rowspan, parse Colspan)

    ||| Extracts the matrix of (rowspan,colspan) dimensions of a <td> element
    public export
    tableDimensions : NodePosition -> List (List (Nat,Nat))
    tableDimensions = rows .> map (cells .> map tdDimensions)

    ||| Given a list of cells and a candidate position, it returns the closest 
    ||| position not in the list within the same row to the right
    public export
    nextFree : (Nat,Nat) -> List (Nat,Nat) -> (Nat,Nat)
    nextFree (i,j) cells =
        if cells `contains` (i,j) then
            assert_total nextFree (i,j+1) cells
        else
            (i,j)

    ||| Given a (x,y) position and (width,height) dimensions, it returns
    ||| the list of cells within the rectangle (i,j) for
    ||| x <= i <= x + width, y <= j <= y + height
    public export
    positions : (Nat,Nat) -> (Nat,Nat) -> List (Nat,Nat)
    positions (i,j) (width,height) =
        [(i+x, j+y) | x <- [0 .. (width `minus` 1)], y <- [0 .. (height `minus` 1)]]

    ||| Given an area with a (x,y) position and (width,height) dimensions,
    ||| and given a list of taken cells, tries to place it in the
    ||| next available position, returning the updated position and list
    ||| of taken cells.
    ||| If the result overlaps, it returns `Nothing`
    public export
    place : (Nat, Nat) -> (Nat, Nat) -> List (Nat,Nat) -> Maybe ((Nat,Nat), List (Nat,Nat))
    place (i,j) (width, height) list =
        let (i', j') = nextFree (i, j) list
            new = positions (i', j') (width, height)
            combined = list ++ new
            in

        if isSet combined then
            Just ((i',j'),combined)
        else
            Nothing

    ||| It tries to place all the cells in a row. If successful, it returns updated
    ||| position and list of taken cells.
    public export
    placeRow : Nat -> List (Nat,Nat) -> List (Nat,Nat) -> Maybe ((Nat,Nat), List (Nat,Nat))
    placeRow i cells list =
        go (i,0) cells list where

        go: (Nat,Nat) -> List (Nat,Nat) -> List (Nat,Nat) -> Maybe ((Nat,Nat), List (Nat,Nat))
        go pos [] list = Just (pos,list)
        go pos (dim::cells) list =
            case place pos dim list of
                Nothing => Nothing
                Just ((i',j'), list') => go (i', j' + 1) cells list'

    ||| It tries to place all the cells in a table. If successful, it returns the
    ||| list of taken cells.
    public export
    placeTable: List (List (Nat,Nat)) -> Maybe (List (Nat,Nat))
    placeTable rows =
        go 0 rows [] where

        go : Nat -> List (List (Nat,Nat)) -> List (Nat,Nat) -> Maybe (List (Nat,Nat))
        go _ [] list = Just list
        go i (row::rows) list =
            case placeRow i row list of
                Nothing => Nothing
                Just ((i',_), list') => go (i'+1) rows list'

    ||| It returns the amount of columns that a <col> element declares
    public export
    columnWidth: NodePosition -> Nat
    columnWidth col =
        case col !! Span of
            Nothing => 1
            Just span =>
                case parsePositive span of
                    Just num => min 1000 (max 1 num)
                    Nothing => 1

    ||| It returns the amount of columns that a <colgroup> element declares
    public export
    colgroupWidth: NodePosition -> Nat
    colgroupWidth colgroup =
        colgroup
        |> children
        |> filter (`is` Col)
        |> \case
            [] => columnWidth colgroup
            cols => cols |> map columnWidth |> sum 
        
            

    ||| It returns the amount of columns that a <table> element declares
    ||| via <colgroup> elements
    public export
    tableWidth: NodePosition -> Nat
    tableWidth =
        children 
        .> filter (`is` Colgroup)
        .> map colgroupWidth
        .> sum


    ||| It returns True if the cells of a table don't overlap
    ||| and there are no empty columns
    public export
    validTable : NodePosition -> Bool
    validTable table =
        table
        |> tableDimensions
        |> placeTable
        |> \case
            Nothing => False
            Just cells => foldr max 0 (map snd cells) >= tableWidth table


namespace Base
    ||| Returns True if the attribute is required to contain a URL value
    public export
    containsURL: Attribute -> Condition -> Bool
    containsURL attr (attr' `Is` URL) = (attr == attr')
    containsURL attr (attr' `IsRequired` URL) = (attr == attr')
    containsURL attr (c && c') = containsURL attr c || containsURL attr c'
    containsURL attr (c || c') = containsURL attr c && containsURL attr c'
    containsURL _ _ = False

    ||| Returns True if any of the element's attributes are required
    ||| to be URLs by the specification
    public export
    hasURL : NodePosition -> Bool 
    hasURL node =
        node
        |> attributes
        |> keys
        |> any (\attr => restrictions node |> containsURL attr)