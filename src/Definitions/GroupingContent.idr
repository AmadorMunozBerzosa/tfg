||| Specification for the elements in section 4.4
||| https://html.spec.whatwg.org/multipage/grouping-content.html
module Definitions.GroupingContent 

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes



public export
specification : Tag -> Maybe Specification

specification P = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Hr = Just <| Spec {
    categories = [ >>> Flow, >>> SelectElementInnerContent],
    contentModel = >>> Nothing
}

specification Pre = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Blockquote = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Flow),
    attributes = [Cite],
    restrictions = Cite `Is` URL
}

specification Ol = Just <| Spec {
    categories = [ >>> Flow, HasChild Li ==> Palpable],
    contentModel = >>> Many (Any [
        Tag Li,
        Category ScriptSupporting
    ]),
    attributes = [Reversed, Start, Type'],
    restrictions = [
        Reversed `Is` Boolean,
        Start `Is` Integer',
        Type' `Is` "decimal" || "a" || "A" || "i" || "I"
    ]
}

specification Ul = Just <| Spec {
    categories = [ >>> Flow, HasChild Li ==> Palpable],
    contentModel = >>> Many (Any [
        Tag Li,
        Category ScriptSupporting
    ])
}

specification Menu = Just <| Spec {
    categories = [ >>> Flow, HasChild Li ==> Palpable],
    contentModel = >>> Many (Any [
        Tag Li,
        Category ScriptSupporting
    ])
}

specification Li = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow),
    attributes = [Value],
    restrictions = [
        Value `Is` Integer',
        Has Value ==> HasParent Ol
    ]
}

specification Dl = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Intermixed ScriptSupporting (Any [
        Many (
            Sequence [
                AtLeastOne (Tag Dt),
                AtLeastOne (Tag Dd)
            ]
        ),
        Many (Tag Div)
    ])
}

specification Dt = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow),
    restrictions = [
        NoDescendant Header,
        NoDescendant Footer,
        NoDescendant Address,
        NoDescendantCategory Heading,
        NoDescendantCategory Sectioning
    ]
}

specification Dd = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow)
}

specification Figure = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Any [
        Sequence [Tag Figcaption, Many (Category Flow)],
        Sequence [Many (Category Flow), Tag Figcaption],
        Sequence [Many (Category Flow)]
    ]
}

specification Figcaption = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow)
}

specification Main = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Flow),
    restrictions = HierarchicallyCorrectMain
}

specification Search = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification Div = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Palpable,
        >>> SelectElementInnerContent,
        >>> OptgroupElementInnerContent,
        >>> OptionElementInnerContent
    ],
    contentModel = When [
        (HasParent Dl,
            Intermixed ScriptSupporting <| Sequence [AtLeastOne (Tag Dt), AtLeastOne (Tag Dd)]
        ),
        (HasAncestor Option,
            Many (Category OptionElementInnerContent)
        ),
        (HasAncestor Optgroup,
            Many (Category OptgroupElementInnerContent)
        ),
        (HasAncestor Select,
            Many (Category SelectElementInnerContent)
        )
    ] (Many (Category Flow))
}

specification _ = Nothing