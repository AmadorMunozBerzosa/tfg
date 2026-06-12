||| Specification for the elements in section 4.5
||| https://html.spec.whatwg.org/multipage/text-level-semantics.html
module Definitions.TextSemantics

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes

public export
specification : Tag -> Maybe Specification

specification A = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Palpable,
        Has Href ==> Interactive
        ],
    contentModel = >>> Transparent Nothing,
    attributes = [ Href, Target, Download, Ping, Rel, Hreflang, Type', Referrerpolicy ],
    restrictions = [
        Href `Is` URL,
        Target `Is` NavigableTargetName || NavigationKeyword,
        Download `Is` Anything,
        Ping `Is` (List " " URL),
        Rel `Is` List " " ("norefferer" || "noopener" || "opener"),
        Hreflang `Is` LanguageCode,
        Type' `Is` MimeType,
        Referrerpolicy `Is` ReferrerPolicy,

        Has Target ==> Has Href,
        Has Download ==> Has Href,
        Has Ping ==> Has Href,
        Has Rel ==> Has Href,
        Has Hreflang ==> Has Href,
        Has Type' ==> Has Href, 
        Has Referrerpolicy ==> Has Href,
        Has Itemprop ==> Has Href,
        NoDescendant A,
        NoDescendantCategory Interactive,
        NoDescendantAttribute Tabindex
    ]
}

specification Em = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing)
}

specification Strong = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing)
}

specification Small = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing)
}

specification S = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing)
}

specification Cite = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing)
}

specification Q = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing),
    attributes = [Cite],
    restrictions = [
        Cite `Is` URL
    ]
}

specification Dfn = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing),
    attributes = [Cite],
    restrictions = [
        Cite `Is` URL,
        NoDescendant Dfn
    ]
}

specification Abbr = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Many (Category Phrasing)
}

specification Ruby = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable ],
    contentModel = >>> Any [
        AtLeastOne <| Any [
            AtLeastOne <| Tag Rt,
            Sequence [
                Tag Rp,
                AtLeastOne <| Sequence [
                    Tag Rt,
                    Tag Rp
                ]
            ]
        ],
        AtLeastOne <| Category Phrasing
    ],
    restrictions = [
        HasChildCategory Phrasing ==> UniqueChild Ruby,
        HasChildCategory Phrasing ==> NoIndirectDescendant Ruby
    ]
}

specification Rt = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Phrasing)
}

specification Rp = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Phrasing)
} 

specification Data = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing),
    attributes = [Value],
    restrictions = [ Value `Is` Anything, Has Value]
}

specification Time = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = When [ (Has Datetime, Many (Category Phrasing))] Text,
    attributes = [Datetime],
    restrictions = [
        Datetime `Is` Datetime,
        Has Datetime ==> Childless
    ]
}

specification Code = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Var = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Samp = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Kbd = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Sub = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Sup = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification I = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification B = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification U = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Mark = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Bdi = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing)
}

specification Bdo = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Many (Category Phrasing),
    restrictions = Has Dir
}

specification Span = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = When [
        (HasAncestor Option, Many (Category OptionElementInnerContent))
    ] (Many (Category Phrasing))
}

specification Br = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing],
    contentModel = >>> Nothing
}

specification Wbr = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing],
    contentModel = >>> Nothing
}

specification _ = Nothing