||| Specification for the elements in sections 4.1 and 4.2
||| https://html.spec.whatwg.org/multipage/semantics.html
module Definitions.DocumentMetadata 

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes

public export
specification : Tag -> Maybe Specification
specification Html = Just <| Spec {
    categories = [],
    contentModel = >>> Sequence [ Tag Head, Tag Body]
}

specification Head = Just <| Spec {
    categories = [],
    contentModel = >>> Many (Category Metadata),
    restrictions = [
        UniqueChild Base, 
        UniqueChild Title,
        NoAncestor Iframe ==> HasChild Title,
        UniqueCharset,
        UniqueTranslationPerLanguage,
        UniqueDescription,
        UniqueThemeColorPerMedia,
        UniqueColorScheme,
        UniqueEncoding,
        UniqueMetaState
    ]   
}

specification Title = Just <| Spec {
    categories =  [ >>> Metadata],
    contentModel = >>> Text,
    restrictions = HasContent
}

specification Base = Just <| Spec {
    categories =  [ >>> Metadata],
    attributes = [Href, Target],
    contentModel = >>> Nothing,
    restrictions = [
        Href `Is` URL,
        Target `Is` NavigableTargetName || NavigationKeyword,
        HasAny [Href, Target],
        BeforeURLs
    ]
}

specification Link = Just <| Spec {
    categories = [
        >>> Metadata,
        Has Itemprop ==> Flow,
        Has Itemprop ==> Phrasing,
        Rel `IsRequired` "dns-prefetch" || "modulepreload" || "pingback" || "preconnect" || "prefetch" || "preload" || "stylesheet" ==> Flow,
        Rel `IsRequired` "dns-prefetch" || "modulepreload" || "pingback" || "preconnect" || "prefetch" || "preload" || "stylesheet" ==> Phrasing
    ],
    contentModel = >>> Nothing,
    attributes = [ Href, Target, Crossorigin, Rel, Media, Integrity, Hreflang, Type', Referrerpolicy, Imagesrcset, Imagesizes, Sizes, As, Blocking, Color, Disabled, Fetchpriority ],
    restrictions = [
        Href `Is` URL,
        Crossorigin `Is` Cors,
        Rel `Is` List " " Anything,
        Media `Is` MediaQuery,
        Integrity `Is` Anything,
        Hreflang `Is` LanguageCode,
        Type' `Is` MimeType,
        Referrerpolicy `Is` ReferrerPolicy,
        Imagesrcset `Is` List "," ImageCandidate,
        Imagesizes `Is` List " " Size,
        Sizes `Is` List " " Size,
        As `Is` ResourceKind,
        Blocking `Is` BlockingFormat,
        Color `Is` Color,
        Disabled `Is` Boolean,
        Fetchpriority `Is` FetchPriority,

        HasAny [Href, Imagesrcset],
        HasAtMostOne [Rel,Itemprop],
        Has Imagesrcset ==> Has Imagesizes,
        Has Sizes ==> Rel `IncludesAny` ["icon", "apple-touch-icon"],
        As `IsRequired` PreloadDestination ==> Rel `IncludesAny` ["preload"],
        As `IsRequired` ModulePreloadDestination ==> Rel `IncludesAny` ["modulepreload"],
        
        Has Blocking ==> Has Rel,
        Has Blocking ==> Rel `IncludesAny` ["stylesheet", "expect"],

        Has Color ==> Has Rel,
        Has Color ==> Rel `IncludesAny` ["mask-icon"],
        
        Has Disabled ==> Has Rel,
        Has Disabled ==> Rel `IncludesAny` ["stylesheet"],
        
        [
            Has Rel,
            Rel `IncludesAny` ["stylesheet"],
            Rel `IncludesAny` ["alternate"]
        ] ==> Has Title
    ]   
}


specification Meta = Just <| Spec {
    categories = [
        >>> Metadata,
        Has Itemprop ==> Flow,
        Has Itemprop ==> Phrasing    
    ],
    contentModel = >>> Nothing,
    attributes = [Name, Value, HttpEquiv, Content, Charset, Media],
    restrictions = [
        Name `Is` NotURL,
        Value `Is` NotURL,
        HttpEquiv `Is` "content-language" || "content-type" || "default-style" || "refresh" || "set-cookie" || "x-ua-compatible" || "content-security-policy",
        Content `Is` Anything,
        Charset `Is` "utf-8",
        Media `Is` MediaQuery,

        HasAtMostOne [Name, HttpEquiv, Charset, Itemprop],
        HasAny [Name, HttpEquiv, Charset, Itemprop],
        
        Has Content ==> HasAny [Name, HttpEquiv, Itemprop],
        Has Name ==> Has Content,
        Has HttpEquiv ==> Has Content,
        Has Itemprop ==> Has Content,

        
        Has Media ==> Name `Is` "theme-color",
        Name `IsRequired` "keywords" ==> Value `Is` List "," Anything,
        Name `IsRequired` "referrer" ==> Value `Is` ReferrerPolicy,
        Name `IsRequired` "theme-color" ==> Value `Is` Color,
        HttpEquiv `IsRequired` "content-type" ==> Value `Is` WhitespaceBetween "text/html;" "charset=utf-8",
        HttpEquiv `IsRequired` "refresh" ==> Value `Is` RefreshPolicy,
        HttpEquiv `IsRequired` "x-ua-compatible" ==> Value `Is` "IE=edge",
        HttpEquiv `IsRequired` "content-security-policy" ==> Value `Is` ContentSecurityPolicy
    ]
}

specification Style = Just <| Spec {
    categories =  [ >>> Metadata],
    contentModel = >>> Text,
    attributes = [Media, Blocking],
    restrictions = [
        Media `Is` MediaQuery,
        Blocking `Is` BlockingFormat
    ]
}

specification _ = Nothing