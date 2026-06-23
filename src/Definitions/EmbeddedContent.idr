||| Specification for the elements in section 4.8
||| https://html.spec.whatwg.org/multipage/embedded-content.html
||| https://html.spec.whatwg.org/multipage/images.html
||| https://html.spec.whatwg.org/multipage/iframe-embed-object.html
||| https://html.spec.whatwg.org/multipage/media.html
||| https://html.spec.whatwg.org/multipage/image-maps.html
||| https://html.spec.whatwg.org/multipage/embedded-content-other.html
module Definitions.EmbeddedContent

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes

public export
specification : Tag -> Maybe Specification

specification Picture = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Embedded, >>> Palpable],
    contentModel = >>> Intermixed ScriptSupporting (Sequence [
        Many (Tag Source),
        Tag Img
    ])
}

specification Source = Just <| Spec {
    categories = [],
    contentModel = >>> Nothing,
    attributes = [ Type', Media, Src, Srcset, Sizes, Width, Height],
    restrictions = [
        Type' `Is` MimeType,
        Media `Is` MediaQuery,
        Src `Is` URL,
        Srcset `Is` List "," ImageCandidate,
        Sizes `Is` List " " Size,
        Width `Is` NonNegative,
        Height `Is` NonNegative,

        HasParent Source ==> Has Srcset,
        [HasParent Source, NextSiblingAuto] ==> Has Sizes,

        [HasParent Source, NextSiblingTag [Img, Source]] ==> (
            HasParent Source || Has Type' || (Media `IsRequired` Trimmed "all")
        ),

        HasParent Audio ==> Has Src,
        HasParent Audio ==> HasNot Srcset,
        HasParent Audio ==> HasNot Sizes,

        HasParent Video ==> Has Src,
        HasParent Video ==> HasNot Srcset,
        HasParent Video ==> HasNot Sizes
    ]
}

specification Img = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Embedded,
        >>> FormAssociated,
        >>> Palpable,
        Has Usemap ==> Interactive,
        Has Controls ==> Interactive
    ],
    contentModel = >>> Nothing,
    attributes = [Alt, Src, Srcset, Sizes, Crossorigin, Usemap, Ismap, Controls, Width, Height, Referrerpolicy, Decoding, Loading, Fetchpriority],
    restrictions = [
        Alt `Is` Anything,
        Src `Is` URL,
        Srcset `Is` List "," ImageCandidate,
        Sizes `Is` List " " Size,
        Crossorigin `Is` Cors,
        Usemap `ReferencesTag` (Map,Id),
        Ismap `Is` Boolean,
        Controls `Is` Boolean,
        Width `Is` NonNegative,
        Height `Is` NonNegative,
        Referrerpolicy `Is` ReferrerPolicy,
        Decoding `Is` "sync" || "async" || "auto",
        Loading `Is` Loading,
        Fetchpriority `Is` FetchPriority,
    
        HasAny [Src, Srcset],

        Sizes `IsRequired` "auto" ==> HasNot Srcset,
        Sizes `IsRequired` "auto" ==> Loading `Is` "lazy",

        Has Ismap ==> HasAncestorAttribute A Href,

        Has Controls ==> Has Alt
    ]
}

specification Iframe = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Embedded,
        >>> Interactive,
        >>> Palpable
    ],
    contentModel = >>> Nothing,
    attributes = [ Src, Srcdoc, Name, Sandbox, Allow, Allowfullscreen, Width, Height, Referrerpolicy, Loading],

    restrictions = [
        Src `Is` URL,
        Srcdoc `Is` SrcDoc,
        Name `Is` NavigableTargetName,
        Sandbox `Is` List " " (
            "allow-downloads" ||
            "allow-forms" ||
            "allow-modals" ||
            "allow-orientation-lock" ||
            "allow-pointer-lock" ||
            "allow-popups-to-escape-sandbox" ||
            "allow-popups" ||
            "allow-presentation" ||
            "allow-same-origin" ||
            "allow-scripts" ||
            "allow-top-navigation-by-user-activation" ||
            "allow-top-navigation-to-custom-protocols" ||
            "allow-top-navigation" ),

        Allow `Is` PermissionsPolicy,
        Allowfullscreen `Is` Boolean,
        Width `Is` NonNegative,
        Height `Is` NonNegative,
        Referrerpolicy `Is` ReferrerPolicy,
        Loading `Is` Loading,

        Sandbox `IncludesNone` ["allow-top-navigation","allow-top-navigation-by-user-activation"],

        Has Itemprop ==> Has Src,
        Sandbox `IncludesRequired` ["allow-top-navigation","allow-popups"]
            ==> Sandbox `IncludesNone` ["allow-top-navigation-to-custom-protocols"]
    ]
}

specification Embed = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Embedded,
        >>> Interactive,
        >>> Palpable
    ],
    contentModel = >>> Nothing,
    attributes = [ Src, Type', Width, Height],

    restrictions = [
        Has Itemprop ==> Has Src,
        Sandbox `IncludesAny` ["allow-top-navigation"]
            ==> Sandbox `IncludesNone` ["allow-top-navigation-by-user-activation"],
        Sandbox `IncludesRequired` ["allow-top-navigation", "allow-popups"]
            ==> Sandbox `IncludesNone` ["allow-top-navigation-to-custom-protocols"]
    ]
}

specification Object = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Embedded,
        >>> FormAssociated,
        >>> Listed,
        >>> Palpable
    ],
    contentModel = >>> Transparent Nothing,
    attributes = [ Data, Type', Name, Form, Width, Height],
    restrictions = [
        Data `Is` URL,
        Type' `Is` MimeType,
        Name `Is` NavigableTargetName,
        Form `ReferencesTag` (Form, Name),
        Width `Is` NonNegative,
        Height `Is` NonNegative
    ]
}

specification Video = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Embedded,
        >>> Palpable,
        Has Controls ==> Interactive
    ],
    contentModel = When [
        (Has Src, Transparent (Many (Tag Track)))
    ] (Transparent (Sequence [Many (Tag Source),Many (Tag Track)])),
    attributes = [ Src, Crossorigin, Poster, Preload, Autoplay, Playsinline, Loop, Muted, Controls, Loading, Width, Height],

    restrictions = [
        Src `Is` URL,
        Crossorigin `Is` Cors,
        Poster `Is` URL,
        Preload `Is` Preload,
        Autoplay `Is` Boolean,
        Playsinline `Is` Boolean,
        Loop `Is` Boolean,
        Muted `Is` Boolean,
        Controls `Is` Boolean,
        Loading `Is` Loading,
        Width `Is` NonNegative,
        Height `Is` NonNegative,
    
        NoAncestor Video,
        NoAncestor Audio,
        UniqueDefaultSubtitles,
        UniqueDefaultDescription,
        UniqueTrackPerLangAndLabel
    ]
}

specification Audio = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Embedded,
        Has Controls ==> Interactive,
        Has Controls ==> Palpable
    ],
    contentModel = When [
        (Has Src, Transparent (Many (Tag Track)))
    ] (Transparent (Sequence [Many (Tag Source),Many (Tag Track)])),
    attributes = [Src,Crossorigin,Preload,Autoplay,Loop,Muted,Controls,Loading],
    restrictions = [
        Src `Is` URL,
        Crossorigin `Is` Cors,
        Preload `Is` Preload,
        Autoplay `Is` Boolean,
        Loop `Is` Boolean,
        Muted `Is` Boolean,
        Controls `Is` Boolean,
        Loading `Is` Loading,
    
        NoAncestor Video,
        NoAncestor Audio,
        UniqueDefaultSubtitles,
        UniqueDefaultDescription,
        UniqueTrackPerLangAndLabel
    ]
}

specification Track = Just <| Spec {
    categories = [],
    contentModel = >>> Nothing,
    attributes = [ Kind, Src, Srclang, Label, Default],
    restrictions = [
        Kind `Is` "subtitles" || "captions" || "descriptions" || "chapters" || "metadata",
        Src `Is` URL,
        Srclang `Is` LanguageCode,
        Label `Is` Anything,
        Default `Is` Boolean,
        Has Src,
        Src `IsRequired` "subtitles" ==> Has Srclang,
        NotEmpty Label
    ]
}

specification Map = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Palpable
    ],
    contentModel = >>> Transparent Nothing,
    attributes = [Name],
    restrictions = [
        Name `Is` NavigableTargetName,
        Has Name ==> Same Name Id
    ]
}

specification  Area = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing
    ],
    contentModel = >>> Nothing,
    attributes = [ Alt, Coords, Shape, Href, Target, Download, Ping, Rel, Referrerpolicy],

    restrictions = [
        Alt `Is` Anything,
        Coords `Is` List "," Float,
        Shape `Is` "circle" || "default" || "poly" || "rect",
        Href `Is` URL,
        Target `Is` NavigableTargetName || NavigationKeyword,
        Download `Is` Anything,
        Ping `Is` List " " URL,
        Rel `Is` List " " ("noreferrer" || "noopener" || "opener"),
        Referrerpolicy `Is` ReferrerPolicy,
        HasAncestor Map,
        Has Href ==> Has Alt,
        Has Alt ==> Has Href,
        Shape `Is` "default" ==> HasNot Coords,

        Shape `Is` "circle" ==> Has Coords,
        Shape `Is` "circle" ==> Coords `Is` CircleCoords,

        Shape `Is` "poly" ==> Has Coords,
        Shape `Is` "poly" ==> Coords `Is` PolygonCoords,

        Shape `Is` "rect" ==> Has Coords,
        Shape `Is` "rect" ==> Coords `Is` RectCoords,

        Has Target ==> Has Href,
        Has Download ==> Has Href,
        Has Ping ==> Has Href,
        Has Rel ==> Has Href,
        Has Referrerpolicy ==> Has Href
    ]
}

specification Math = Just <| Spec {
    categories = [ >>> Embedded, >>> Phrasing, >>> Flow, >>> Palpable],
    contentModel = >>> Anything
}

specification SVG = Just <| Spec {
    categories = [ >>> Embedded, >>> Phrasing, >>> Flow, >>> Palpable],
    contentModel = >>> Anything
}

specification _ = Nothing