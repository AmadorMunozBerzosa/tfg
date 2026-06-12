||| Miscellaneous functions on List
module Util.List

import Data.String
import Util.Map
import Data.Maybe
import Data.String.Extra

||| Returns True if exactly one element in the list meets the predicate
public export
unique : (a -> Bool) -> List a -> Bool
unique pred list =
    go False list where 
    -- The boolean flag is True if an element meeting the predicate has already been found
    go: Bool -> List a -> Bool
    go _ [] = True 
    go flag (x::xs) =
        if pred x then
            if flag then False else go True xs
        else
            go flag xs

||| Returns true if the value is an element of the list
public export
contains: Eq a => List a -> a -> Bool
contains [] _ = False
contains (x::xs) a = if a == x then True else contains xs a

||| Returns True if the list has no repeated elements
public export
isSet: Eq a => List a -> Bool
isSet = go [] where
    go : List a -> List a -> Bool
    go aux [] = True
    go aux (x::xs) = (not (contains aux x)) && go (x::aux) xs

||| Classifies the elements of a list into two lists
public export
classify: List (Either a b) -> (List a, List b)
classify [] = ([], [])
classify (x::xs) =
    let (as,bs) = classify xs in

    case x of
        Left a => (a::as, bs)
        Right b => (as, b::bs)


||| Converts a list of pairs into a pair of lists
public export
split: List (a,b) -> (List a, List b)
split [] = ([], [])
split ((a,b)::xs) =
    let (as,bs) = split xs in

    (a::as, b::bs)

||| Given a value, it tries to apply a list of function until one of them
||| returns `Just`. If none, it returns a default value
public export
tryUntilJust : a -> List (a -> Maybe b) -> b -> b
tryUntilJust _ [] b = b
tryUntilJust a (f::fs) b =
    case f a of
        Just b => b
        Nothing => tryUntilJust a fs b

||| Transforms a list of optional values to an optional list of values
||| The result is `Just` if all elements in the input are
public export
combine: List (Maybe a) -> Maybe (List a)
combine [] = Just []
combine (Nothing::_) = Nothing
combine (Just a :: as) = map (a::) (combine as)