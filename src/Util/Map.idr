||| Elementary reimplementation of the Data.Map module so that Idris
||| will normalize calls to its functions during proof checking
module Util.Map

import Data.Maybe

||| A map is isomorphic to a list of (key,value) pairs
public export
data Map: (key:Type) -> Type -> Type where
    Nil: Map key value
    (::) : (key, value) -> Map key value -> Map key value

||| The empty map
public export
empty: Map key value
empty = []

||| The list of keys the map contains
public export
keys: Map key value -> List key
keys [] = []
keys ((key, _) :: map) = key :: keys map

||| `Just value` if the given key is found in the map
||| `Nothing` otherwise
public export
lookup: Eq key => key -> Map key value -> Maybe value
lookup _ [] = Nothing
lookup key ((key',value)::map) =
    if key == key' then
        Just value
    else
        lookup key map

||| Returns True if the key is found in the map
public export
has: Eq key => Map key value -> key -> Bool
has map key = isJust (lookup key map)

||| Forms a map from a list of (key,value) pairs
public export
fromList: List (key,value) -> Map key value
fromList = foldr (::) []

||| Forms a list of (key,value) pairs from a map
public export
toList: Map key value -> List (key,value)
toList map = go map [] where
    go: Map key value -> List (key,value) -> List (key,value)
    go [] list = list
    go ((key,value)::xs) list = go xs ((key,value)::list)