||| Specification for the elements in section 4.3
||| https://html.spec.whatwg.org/multipage/sections.html
module Definitions.Sections 

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes


public export
specification : Tag -> Maybe Specification
specification Body = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Flow),
    attributes = [ Onafterprint, Onbeforeprint, Onbeforeunload, Onhashchange, Onlanguagechange, Onmessage, Onmessageerror, Onoffline, Ononline, Onpageswap, Onpagehide, Onpagereveal, Onpageshow, Onpopstate, Onrejectionhandled, Onstorage, Onunhandledrejection, Onunload ],
    restrictions = [
        CorrectHeadingLevel,
        Onafterprint `Is` EventHandler,
        Onbeforeprint `Is` EventHandler,
        Onbeforeunload `Is` EventHandler,
        Onhashchange `Is` EventHandler,
        Onlanguagechange `Is` EventHandler,
        Onmessage `Is` EventHandler,
        Onmessageerror `Is` EventHandler,
        Onoffline `Is` EventHandler,
        Ononline `Is` EventHandler,
        Onpageswap `Is` EventHandler,
        Onpagehide `Is` EventHandler,
        Onpagereveal `Is` EventHandler,
        Onpageshow `Is` EventHandler,
        Onpopstate `Is` EventHandler,
        Onrejectionhandled `Is` EventHandler,
        Onstorage `Is` EventHandler,
        Onunhandledrejection `Is` EventHandler,
        Onunload `Is` EventHandler
    ]
}

specification Article = Just <| Spec {
    categories = [ >>> Flow, >>> Sectioning, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification Section = Just <| Spec {
    categories = [ >>> Flow, >>> Sectioning, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification Nav = Just <| Spec {
    categories = [ >>> Flow, >>> Sectioning, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification Aside = Just <| Spec {
    categories = [ >>> Flow, >>> Sectioning, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification H1 = Just <| Spec {
    categories = [ >>> Flow, >>> Heading, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification H2 = Just <| Spec {
    categories = [ >>> Flow, >>> Heading, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification H3 = Just <| Spec {
    categories = [ >>> Flow, >>> Heading, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification H4 = Just <| Spec {
    categories = [ >>> Flow, >>> Heading, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification H5 = Just <| Spec {
    categories = [ >>> Flow, >>> Heading, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification H6 = Just <| Spec {
    categories = [ >>> Flow, >>> Heading, >>> Palpable],
    contentModel = >>> Many (Category Flow)
}

specification Hgroup = Just <| Spec {
    categories = [ >>> Flow, >>> Sectioning, >>> Palpable],
    contentModel = >>> Sequence [
        Many (Tag P),
        Any [Tag H1, Tag H2, Tag H3, Tag H4, Tag H5, Tag H6],
        Many (Tag P)
    ]
}

specification Header = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Flow),
    restrictions = [
        NoDescendant Header,
        NoDescendant Footer
    ]
}

specification Footer = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Flow),
    restrictions = [
        NoDescendant Header,
        NoDescendant Footer
    ]
}

specification Address = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Flow),
    restrictions = [
        NoDescendant Header,
        NoDescendant Footer,
        NoDescendant Address,
        NoDescendantCategory Heading,
        NoDescendantCategory Sectioning
    ]
}

specification _ = Nothing