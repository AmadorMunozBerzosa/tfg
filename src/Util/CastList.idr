||| Implements the `CastList` type.
||| `CastList a` is functionally similar to `List a`.
||| Syntactically, however, it allows to write list literals by mixing
||| any types that can be cast to a
module Util.CastList

||| List of values that can be cast to a fixed type
public export
data CastList : Type -> Type where
    Nil: CastList a
    (::): Cast b a => b -> CastList a -> CastList a

||| Casts all the values into a regular list
public export
toList: CastList a -> List a
toList [] = []
toList (x::xs) = cast x :: toList xs