||| Defines a left-to-right composition operator
module Util.Application

infixr 9 .>

||| Right to left composition (i.e. equivalent to `flip (.)`)
public export
(.>): (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)