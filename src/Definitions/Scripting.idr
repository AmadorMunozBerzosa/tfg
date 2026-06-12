||| Specification for the elements in section 4.12
||| https://html.spec.whatwg.org/multipage/scripting.html
||| https://html.spec.whatwg.org/multipage/canvas.html
module Definitions.Scripting

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes

public export
specification : Tag -> Maybe Specification

specification Script = Just <| Spec {
    categories = [ >>> Metadata, >>> Flow, >>> Phrasing, >>> ScriptSupporting],
    contentModel = >>> Text,
    attributes = [
        Type',
        Src,
        Nomodule,
        Async,
        Defer,
        Blocking,
        Crossorigin,
        Referrerpolicy,
        Integrity,
        Fetchpriority
    ],

    restrictions = [
        Type' `Is` MimeType || "module" || "importmap" || "speculationrules" || "",
        Src `Is` URL,

        Has Src ==> Type' `Is` JSMimeType || "module" || "",

        [Type' `IsRequired` JSMimeType || "", HasNot Src] ==> HasNot Async,
        [Type' `IsRequired` JSMimeType || "", HasNot Src] ==> HasNot Defer,
        [Type' `IsRequired` JSMimeType || "", HasNot Src] ==> HasNot Blocking,
        [Type' `IsRequired` JSMimeType || "", HasNot Src] ==> HasNot Integrity,
        [Type' `IsRequired` JSMimeType || "", HasNot Src] ==> HasNot Fetchpriority,
        
        [Type' `IsRequired` "module", Has Src] ==> HasNot Nomodule,
        [Type' `IsRequired` "module", Has Src] ==> HasNot Defer,

        [Type' `IsRequired` "module", HasNot Src] ==> HasNot Nomodule,
        [Type' `IsRequired` "module", HasNot Src] ==> HasNot Defer,
        [Type' `IsRequired` "module", HasNot Src] ==> HasNot Blocking,
        [Type' `IsRequired` "module", HasNot Src] ==> HasNot Integrity,
        [Type' `IsRequired` "module", HasNot Src] ==> HasNot Fetchpriority,

        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Nomodule,
        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Async,
        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Defer,
        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Blocking,
        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Crossorigin,
        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Referrerpolicy,
        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Integrity,
        Type' `IsRequiredNot` JSMimeType || "module" || "" ==> HasNot Fetchpriority,

        Nomodule `Is` Boolean,
        Async `Is` Boolean,
        Defer `Is` Boolean,
        Blocking `Is` BlockingFormat,
        Crossorigin `Is` Cors,
        Referrerpolicy `Is` ReferrerPolicy,
        Integrity `Is` Integrity
    ]
}

specification Noscript = Just <| Spec {
    categories = [
        >>> Metadata,
        >>> Flow,
        >>> Phrasing,
        >>> SelectElementInnerContent,
        >>> OptgroupElementInnerContent
    ],

    contentModel = When [
        (HasAncestor Head,
            Many (Any [Tag Link, Tag Style, Tag Meta])
        )
    ] (Transparent Nothing),

    restrictions = [
        NoDescendant Noscript
    ]
}

specification Template = Just <| Spec {
    categories = [
        >>> Metadata,
        >>> Flow,
        >>> Phrasing,
        >>> ScriptSupporting
    ],
    contentModel = >>> Anything,
    attributes = [
        Shadowrootmode,
        Shadowrootdelegatesfocus,
        Shadowrootslotassignment,
        Shadowrootclonable,
        Shadowrootserializable,
        Shadowrootcustomelementregistry
    ],
    restrictions = [
        Shadowrootmode `Is` "open" || "closed",
        Shadowrootdelegatesfocus `Is` Boolean,
        Shadowrootslotassignment `Is` "named" || "manual",
        Shadowrootclonable `Is` Boolean,
        Shadowrootserializable `Is` Boolean,
        Shadowrootcustomelementregistry `Is` Boolean
    ]
}


specification Slot = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing ],
    contentModel = >>> Transparent Nothing,
    attributes = [Name],
    restrictions = [
        Name `Is` Anything
    ]
}

specification Canvas = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Embedded, >>> Palpable],
    contentModel = >>> Transparent Nothing,
    attributes = [Width, Height],
    restrictions = [
        Width `Is` NonNegative,
        Height `Is` NonNegative,
        Has Id,
        NoDescendantCategory Interactive
    ]
}

specification _ = Nothing