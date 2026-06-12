||| Specification for the elements in section 4.9
||| https://html.spec.whatwg.org/multipage/tables.html
module Definitions.TabularData 

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes

public export
specification : Tag -> Maybe Specification
specification Table = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Intermixed ScriptSupporting (Sequence [
        Optional (Tag Caption),
        Many (Tag Colgroup),
        Optional (Tag Thead),
        Many (Any [
            Tag Tbody,
            Tag Tr
        ]),
        Optional (Tag Tfoot)
    ]),
    restrictions = [
        HasChild Tbody ==> NoChild Tr,
        NoChild Tbody ==> HasChild Tr,
        CorrectTableModel
    ]
}

specification Caption = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow),
    restrictions = [
        NoDescendant Table
    ]
}

specification Colgroup = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Any [ Tag Col, Tag Template]),
    attributes = [Span],
    restrictions = [
        Span `Is` NonNegative,
        Span <= 1000,
        Has Span ==> Childless
    ]
}

specification Col = Just <| Spec {
    categories = [],
    contentModel = >>> Nothing,
    attributes = [Span],
    restrictions = [
        Span <= 1000
    ]
}

specification Tbody = Just <| Spec {
    categories = [],
    contentModel = >>> Many(Any [
        Tag Tr,
        Category ScriptSupporting
    ])
}

specification Thead = Just <| Spec {
    categories = [],
    contentModel = >>> Many(Any [
        Tag Tr,
        Category ScriptSupporting
    ])
}

specification Tfoot = Just <| Spec { 
    categories = [],
    contentModel = >>> Many(Any [
        Tag Tr,
        Category ScriptSupporting
    ])
}

specification Tr = Just <| Spec {
    categories = [],
    contentModel = >>> Many(Any [
        Tag Td,
        Tag Th,
        Category ScriptSupporting
    ])
}

specification Td = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow),
    attributes = [Colspan, Rowspan, Headers],
    restrictions = [
        Colspan <= 1000,
        Rowspan <= 65534,
        EachReferencesTh Headers
    ]
}

specification Th = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow),
    attributes = [Colspan, Rowspan, Headers, Scope, Abbr],
    restrictions = [
        Colspan <= 1000,
        Rowspan <= 65534,
        Scope `Is` "row" || "col" || "rowgroup" || "colgroup",
        Abbr `Is` Anything,

        EachReferencesTh Headers,
        NoDescendant Header,
        NoDescendant Footer,
        NoDescendantCategory Sectioning,
        NoDescendantCategory Heading
    ]
}

specification _ = Nothing
