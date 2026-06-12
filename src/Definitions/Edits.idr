||| Specification for the elements in section 4.7
||| https://html.spec.whatwg.org/multipage/edits.html
module Definitions.Edits 

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes


public export
specification : (t:Tag) -> Maybe Specification
specification Ins = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Transparent Nothing,
    attributes = [Cite, Datetime],
    restrictions = [
        Cite `Is` URL,
        Datetime `Is` Datetime
    ]
}

specification Del = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Palpable],
    contentModel = >>> Transparent Nothing,
    attributes = [Cite, Datetime],
    restrictions = [
        Cite `Is` URL,
        Datetime `Is` Datetime
    ]
}

specification _ = Nothing