||| Specification for the elements in section 4.11
||| https://html.spec.whatwg.org/multipage/interactive-elements.html
module Definitions.InteractiveElements

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes

public export
specification : Tag -> Maybe Specification

specification Details = Just <| Spec {
    categories = [ >>> Flow, >>> Interactive, >>> Palpable],

    contentModel = >>> Sequence [
        Tag Summary,
        Many (Category Flow)
    ],

    attributes = [Name, Open],

    restrictions = [
        NotEmpty Name,
        Open `Is` Boolean,
        UniqueOpenPerGroup,
        NotNestedSameName
    ]
}

specification Summary = Just <| Spec {
    categories = [],
    contentModel = >>> Intermixed Heading (Category Phrasing)
}

specification Dialog = Just <| Spec {
    categories = [ >>> Flow],
    contentModel = >>> Many (Category Flow),
    attributes = [ClosedBy, Open],
    restrictions = [
        Open `Is` Boolean,
        ClosedBy `Is` "any" || "closerequest" || "none",
        HasNot Tabindex
    ]
}

specification _ = Nothing