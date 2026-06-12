||| Collection of proofs about constraint checking

import DSL
import Implementation

public export
actions: List Nat -> List (NodePosition -> Maybe NodePosition)
actions list = list >>= (\n => child :: replicate n next)

public export
(!!): NodePosition -> List Nat -> Maybe NodePosition
node !! list = foldl (>>=) (Just (root node)) (actions list)

valid: Condition -> {default [] i:List Nat} -> Node -> Type 
valid condition {i} node =
    case fromTree node !! i of
        Just node' => errors condition node' === []
        Nothing => Void
    

error: Condition -> {default [] i:List Nat} -> Node -> Type 
error condition {i} node =
    case fromTree node !! i of
        Just node' => Not(errors condition node' === [])
        Nothing => Void

namespace Proof
    public export
    trivial: x = x
    trivial = Refl

namespace ContraProof
    public export
    trivial: Prelude.Uninhabited t => t -> o
    trivial = absurd

-- True -- Always True
-- (&&) Condition Condition -- Conjunction
-- (==>) Condition Condition -- Implication
-- (||) Condition Condition -- Disjunction

(&&): (
    valid (Tag P && Has Id) (p [id ""]),
    error (Tag P && Has Id) (p []),
    error (Tag P && Has Id) (a [id ""]),
    error (Tag P && Has Id) (a [])
    )
(&&) = (trivial, trivial, trivial, trivial)

(||): (
    valid (Tag P || Has Id) (p [id ""]),
    valid (Tag P || Has Id) (p []),
    valid (Tag P || Has Id) (a [id ""]),
    error (Tag P || Has Id) (a [])
    )
(||) = (trivial, trivial, trivial, trivial)

(==>): (
    valid (Tag P ==> Has Id) (p [id ""]),
    error (Tag P ==> Has Id) (p []),
    valid (Tag P ==> Has Id) (a [id ""]),
    valid (Tag P ==> Has Id) (a [])
    )
(==>) = (trivial, trivial, trivial, trivial)


tag : (
    valid (Tag P) (p []),
    error (Tag (Custom "p")) (p []),
    error (Tag P) (a []),
    error (Tag P) (custom "p" [])
    )
tag = (trivial, trivial, trivial, trivial)

category : (
    valid (Category ScriptSupporting) (script []),
    error (Category ScriptSupporting) (p [])
    )
category = (trivial, trivial)

hasContent : (
    valid HasContent (p [p []] ),
    valid HasContent (p ["text"] ),
    error HasContent (p []),
    error HasContent (p ["\n", "  ", comment "comment"])
    )
hasContent = (trivial, trivial, trivial, trivial)

childless : (
    valid Childless (p []),
    valid Childless (p ["\n", "  ", comment "comment"]),
    valid Childless (p ["text"] ),
    error Childless (p [p []] )
    )
childless = (trivial, trivial, trivial, trivial)

validtagname: (
    valid ValidTagName (custom "my-elem" []),
    error ValidTagName (custom "elem" []),
    error ValidTagName (custom "-elem" [])
    )
validtagname = (trivial,trivial,trivial)

has: (
    valid (Has Class) (p [class "name"]),
    valid (Has Class) (p [class ""]),
    error (Has Class) (p [])
    )
has = (trivial, trivial, trivial)

hasNot: (
    error (HasNot Class) (p [class "name"]),
    error (HasNot Class) (p [class ""]),
    valid (HasNot Class) (p [])
    )
hasNot = (trivial, trivial, trivial)

hasAny: (
    valid (HasAny [Class, Id, Name]) (p [class "name"]),
    valid (HasAny [Class, Id, Name]) (p [id "name"]),
    valid (HasAny [Class, Id, Name]) (p [name "name"]),
    error (HasAny [Class, Id, Name]) (p [])
    )
hasAny = (trivial, trivial, trivial, trivial)

hasNone: (
    error (HasNone [Class, Id, Name]) (p [class "name"]),
    error (HasNone [Class, Id, Name]) (p [id "name"]),
    error (HasNone [Class, Id, Name]) (p [name "name"]),
    valid (HasNone [Class, Id, Name]) (p [])
    )
hasNone = (trivial, trivial, trivial, trivial)

hasAtMostOne: (
    valid (HasAtMostOne [Class, Id, Name]) (p [class "name"]),
    valid (HasAtMostOne [Class, Id, Name]) (p [id "name"]),
    valid (HasAtMostOne [Class, Id, Name]) (p [name "name"]),
    valid (HasAtMostOne [Class, Id, Name]) (p []),
    error (HasAtMostOne [Class, Id, Name]) (p [class "name", id "name"]),
    error (HasAtMostOne [Class, Id, Name]) (p [class "name", name "name"]),
    error (HasAtMostOne [Class, Id, Name]) (p [name "name", id "name"])
    )
hasAtMostOne = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

notEmpty: (
    valid (NotEmpty Class) (p []),
    valid (NotEmpty Class) (p [class "name"]),
    error (NotEmpty Class) (p [class ""])
    )
notEmpty = (trivial, trivial, trivial)

is: (
    valid (Is Class Float) (p []),
    valid (Is Class Float) (p [class "2.3"]),
    error (Is Class Float) (p [class "name"])
    )
is = (trivial, trivial, trivial)

isRequired: (
    error (IsRequired Class Float) (p []),
    valid (IsRequired Class Float) (p [class "2.3"]),
    error (IsRequired Class Float) (p [class "name"])
    )
isRequired = (trivial, trivial, trivial)

isNot: (
    valid (IsNot Class "name") (p []),
    valid (IsNot Class "name") (p [class "2.3"]),
    error (IsNot Class "name") (p [class "name"]),
    error (IsNot Class "name") (p [class "nAmE"])
    )
isNot = (trivial, trivial, trivial, trivial)

isRequiredNot: (
    error (IsRequiredNot Class Float) (p []),
    error (IsRequiredNot Class Float) (p [class "2.3"]),
    valid (IsRequiredNot Class Float) (p [class "name"])
    )
isRequiredNot = (trivial, trivial, trivial)

includesAny: (
    valid (IncludesAny Class ["a","b","c"]) (p []),
    valid (IncludesAny Class ["a","b","c"]) (p [class "a"]),
    valid (IncludesAny Class ["a","b","c"]) (p [class "1 a 3"]),
    valid (IncludesAny Class ["a","b","c"]) (p [class "1 2 a"]),
    valid (IncludesAny Class ["a","b","c"]) (p [class "b"]),
    valid (IncludesAny Class ["a","b","c"]) (p [class "c"]),
    valid (IncludesAny Class ["a","b","c"]) (p [class "A"]),
    error (IncludesAny Class ["a","b","c"]) (p [class "1 2 3"])
    )
includesAny = (trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial)

includesRequired: (
    error (IncludesRequired Class ["a","b","c"]) (p []),
    valid (IncludesRequired Class ["a","b","c"]) (p [class "a"]),
    valid (IncludesRequired Class ["a","b","c"]) (p [class "1 a 3"]),
    valid (IncludesRequired Class ["a","b","c"]) (p [class "1 2 a"]),
    valid (IncludesRequired Class ["a","b","c"]) (p [class "b"]),
    valid (IncludesRequired Class ["a","b","c"]) (p [class "c"]),
    valid (IncludesRequired Class ["a","b","c"]) (p [class "A"]),
    error (IncludesRequired Class ["a","b","c"]) (p [class "1 2 3"])
    )
includesRequired = (trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial)

includesNone: (
    valid (IncludesNone Class ["a","b","c"]) (p []),
    error (IncludesNone Class ["a","b","c"]) (p [class "a"]),
    error (IncludesNone Class ["a","b","c"]) (p [class "1 a 3"]),
    error (IncludesNone Class ["a","b","c"]) (p [class "1 2 a"]),
    error (IncludesNone Class ["a","b","c"]) (p [class "b"]),
    error (IncludesNone Class ["a","b","c"]) (p [class "c"]),
    error (IncludesNone Class ["a","b","c"]) (p [class "A"]),
    valid (IncludesNone Class ["a","b","c"]) (p [class "1 2 3"])
    )
includesNone = (trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial)

same: (
    valid (Same Class Id) (p []),
    error (Same Class Id) (p [class "name"]),
    error (Same Class Id) (p [id "name"]),
    valid (Same Class Id) (p [class "name", id "name"]),
    valid (Same Class Id) (p [class "name", id "nAmE"])
    )
same = (trivial, trivial, trivial, trivial, trivial)


(<=): (
    valid (Class <= Id) (p []),
    valid (Class <= Id) (p [class "2"]),
    valid (Class <= Id) (p [id ""]),
    valid (Class <= Id) (p [class "1", id "2"]),
    valid (Class <= Id) (p [class "2", id "1e2"]),
    valid (Class <= Id) (p [class "-2", id "-1"]),
    error (Class <= Id) (p [class "2", id "1"])
    )
(<=) = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

(>=): (
    valid (Id >= Class) (p []),
    valid (Id >= Class) (p [class "2"]),
    valid (Id >= Class) (p [id ""]),
    valid (Id >= Class) (p [class "1", id "2"]),
    valid (Id >= Class) (p [class "2", id "1e2"]),
    valid (Id >= Class) (p [class "-2", id "-1"]),
    error (Id >= Class) (p [class "2", id "1"])
    )
(>=) = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

hasChild: (
    valid (HasChild A) (p [a []]),
    valid (HasChild A) (p [p [], a []]),
    valid (HasChild A) (p [p [], a [], p []]),
    valid (HasChild A) (p [p [], a [], a []]),
    error (HasChild A) (p []),
    error (HasChild A) (p [p []])
    )
hasChild = (trivial, trivial, trivial, trivial, trivial, trivial)

noChild: (
    error (NoChild A) (p [a []]),
    error (NoChild A) (p [p [], a []]),
    error (NoChild A) (p [p [], a [], p []]),
    error (NoChild A) (p [p [], a [], a []]),
    valid (NoChild A) (p []),
    valid (NoChild A) (p [p []])
    )
noChild = (trivial, trivial, trivial, trivial, trivial, trivial)

hasChildCategory: (
    valid (HasChildCategory ScriptSupporting) (p [script []]),
    valid (HasChildCategory ScriptSupporting) (p [p [], script []]),
    valid (HasChildCategory ScriptSupporting) (p [p [], script [], p []]),
    valid (HasChildCategory ScriptSupporting) (p [p [], script [], script []]),
    error (HasChildCategory ScriptSupporting) (p []),
    error (HasChildCategory ScriptSupporting) (p [p []])
    )
hasChildCategory = (trivial, trivial, trivial, trivial, trivial, trivial)

uniqueChild: (
    valid (UniqueChild A) (p [a []]),
    valid (UniqueChild A) (p [p [], a []]),
    valid (UniqueChild A) (p [p [], a [], p []]),
    error (UniqueChild A) (p [p [], a [], a []]),
    valid (UniqueChild A) (p []),
    valid (UniqueChild A) (p [p []])
    )
uniqueChild = (trivial, trivial, trivial, trivial, trivial, trivial)

uniqueDefaultSubtitles: (
    valid UniqueDefaultSubtitles (p []),
    valid UniqueDefaultSubtitles (p [track []]),
    valid UniqueDefaultSubtitles (p [track [kind "subtitles", default' ""]]),
    error UniqueDefaultSubtitles (p [track [kind "subtitles", default' ""], track [kind "subtitles", default' ""]]),
    error UniqueDefaultSubtitles (p [track [kind "subtitles", default' ""], track [kind "captions", default' ""]]),
    valid UniqueDefaultSubtitles (p [track [kind "captions", default' ""], track [default' ""]])
    )
uniqueDefaultSubtitles = (trivial, trivial, trivial, trivial, trivial, trivial)

uniqueDefaultDescription: (
    valid UniqueDefaultDescription (p []),
    valid UniqueDefaultDescription (p [track []]),
    valid UniqueDefaultDescription (p [track [kind "descriptions", default' ""]]),
    error UniqueDefaultDescription (p [track [kind "descriptions", default' ""], track [kind "descriptions", default' ""]]),
    valid UniqueDefaultDescription (p [track [kind "captions", default' ""], track [default' ""]])
    )
uniqueDefaultDescription = (trivial, trivial, trivial, trivial, trivial)

unique: (
    valid (Unique Id) {i=[1]} (p [p [], p [], p [p [id "b"]]]),
    valid (Unique Id) {i=[1]} (p [p [], p [id "a"], p [p [id "b"]]]),
    valid (Unique Id) {i=[1]} (p [p [], p [id "a"], p [p [id "A"]]]),
    error (Unique Id) {i=[1]} (p [p [], p [id "a"], p [p [id "a"]]]),
    error (Unique Id) {i=[1]} (p [p [id "a"], p [id "a"], p [p []]])
    )
unique = (trivial, trivial, trivial, trivial, trivial)

uniqueTag: (
    valid (UniqueTag Id) {i=[1]} (p [p [], p [], p [p [id "b"]]]),
    valid (UniqueTag Id) {i=[1]} (p [p [], p [id "a"], p [p [id "b"]]]),
    valid (UniqueTag Id) {i=[1]} (p [p [], p [id "a"], p [p [id "A"]]]),
    error (UniqueTag Id) {i=[1]} (p [p [], p [id "a"], p [p [id "a"]]]),
    error (UniqueTag Id) {i=[1]} (p [p [id "a"], p [id "a"], p [p []]]),
    valid (UniqueTag Id) {i=[1]} (p [p [], p [id "a"], p [form [id "a"]]]),
    valid (UniqueTag Id) {i=[1]} (p [form [id "a"], p [id "a"], p [p []]])
    )
uniqueTag = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

references: (
    error (References For Id) {i=[1]} (p [p [], p [for "a"], p [p [id "b"]]]),
    error (References For Id) {i=[1]} (p [p [], p [for "a"], p [p [id "A"]]]),
    valid (References For Id) {i=[1]} (p [p [], p [for "a"], p [p [id "a"]]]),
    valid (References For Id) {i=[1]} (p [p [id "a"], p [for "a"], p [p []]])
    )
references = (trivial, trivial, trivial, trivial)

referencesTag: (
    error (ReferencesTag For (Form, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "b"]]]),
    error (ReferencesTag For (Form, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "A"]]]),
    error (ReferencesTag For (Form, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "a"]]]),
    error (ReferencesTag For (Form, Id)) {i=[1]} (p [p [id "a"], p [for "a"], p [p []]]),
    valid (ReferencesTag For (Form, Id)) {i=[1]} (p [p [], p [for "a"], p [form [id "a"]]]),
    valid (ReferencesTag For (Form, Id)) {i=[1]} (p [form [id "a"], p [for "a"], p [p []]])
    )
referencesTag = (trivial, trivial, trivial, trivial, trivial, trivial)

referencesAttribute: (
    error (ReferencesAttribute For (Class, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "b"]]]),
    error (ReferencesAttribute For (Class, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "A"]]]),
    error (ReferencesAttribute For (Class, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "a"]]]),
    error (ReferencesAttribute For (Class, Id)) {i=[1]} (p [p [id "a"], p [for "a"], p [p []]]),
    valid (ReferencesAttribute For (Class, Id)) {i=[1]} (p [p [], p [for "a"], p [p [class "", id "a"]]]),
    valid (ReferencesAttribute For (Class, Id)) {i=[1]} (p [p [class "", id "a"], p [for "a"], p [p []]])
    )
referencesAttribute = (trivial, trivial, trivial, trivial, trivial, trivial)

referencesCategory: (
    error (ReferencesCategory For (ScriptSupporting, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "b"]]]),
    error (ReferencesCategory For (ScriptSupporting, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "A"]]]),
    error (ReferencesCategory For (ScriptSupporting, Id)) {i=[1]} (p [p [], p [for "a"], p [p [id "a"]]]),
    error (ReferencesCategory For (ScriptSupporting, Id)) {i=[1]} (p [p [id "a"], p [for "a"], p [p []]]),
    valid (ReferencesCategory For (ScriptSupporting, Id)) {i=[1]} (p [p [], p [for "a"], p [script [id "a"]]]),
    valid (ReferencesCategory For (ScriptSupporting, Id)) {i=[1]} (p [script [id "a"], p [for "a"], p [p []]])
    )
referencesCategory = (trivial, trivial, trivial, trivial, trivial, trivial)

eachReferencesId: (
    error (EachReferencesId For) {i=[1]} (p [p [], p [for "a"], p [p [id "b"]]]),
    error (EachReferencesId For) {i=[1]} (p [p [], p [for "a"], p [p [id "A"]]]),
    valid (EachReferencesId For) {i=[1]} (p [p [], p [for "a"], p [p [id "a"]]]),
    valid (EachReferencesId For) {i=[1]} (p [p [id "a"], p [for "a"], p [p []]]),
    error (EachReferencesId For) {i=[1]} (p [p [], p [for "a b"], p [p [id "a"]]]),
    valid (EachReferencesId For) {i=[1]} (p [p [id "b"], p [for "a b"], p [p [id "a"]]])
    )
eachReferencesId = (trivial, trivial, trivial, trivial, trivial, trivial)

eachReferencesTh: (
    error (EachReferencesTh For) {i=[0,1]} (p [ table [th [], p [for "a"], th []] ]),
    valid (EachReferencesTh For) {i=[0,1]} (p [ table [th [id "a"], p [for "a"], th []] ]),
    error (EachReferencesTh For) {i=[0,1]} (p [ table [th [], p [for "a"], th [], td [id "a"]] ]),
    valid (EachReferencesTh For) {i=[0,1]} (p [ table [th [id "b"], p [for "a b"], th [id "a"]] ]),
    error (EachReferencesTh For) {i=[0,1]} (p [ table [th [id "b"], p [for "a"]], table [th [id "a"]] ])
    )
eachReferencesTh = (trivial, trivial, trivial, trivial, trivial)

hasParent: (
    valid (HasParent P) {i=[0]} (p [ a []]),
    error (HasParent P) {i=[0]} (div [ a []])
    )
hasParent = (trivial, trivial)

notParent: (
    error (NotParent P) {i=[0]} (p [ a []]),
    valid (NotParent P) {i=[0]} (div [ a []])
    )
notParent = (trivial, trivial)

nextSiblingAuto: (
    error NextSiblingAuto {i=[0]} (p [ a []]),
    error NextSiblingAuto {i=[0]} (p [ a [], img []]),
    error NextSiblingAuto {i=[0]} (p [ a [], img [loading "lazy"]]),
    error NextSiblingAuto {i=[0]} (p [ a [], img [sizes "auto"]]),
    valid NextSiblingAuto {i=[0]} (p [ a [], img [loading "lazy", sizes "auto"]]),
    valid NextSiblingAuto {i=[0]} (p [ a [], img [loading "lazy", sizes "auto,120"]]),
    error NextSiblingAuto {i=[0]} (p [ a [], p [], img [loading "lazy", sizes "auto,120"]])
    )
nextSiblingAuto = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

nextSiblingTag: (
    error (NextSiblingTag [P,A,I]) {i=[0]} (p [ a []]),
    error (NextSiblingTag [P,A,I]) {i=[0]} (p [ a [], div []]),
    valid (NextSiblingTag [P,A,I]) {i=[0]} (p [ a [], p []]),
    valid (NextSiblingTag [P,A,I]) {i=[0]} (p [ a [], a []]),
    valid (NextSiblingTag [P,A,I]) {i=[0]} (p [ a [], i []])
    )
nextSiblingTag = (trivial, trivial, trivial, trivial, trivial)

hasAncestor: (
    valid (HasAncestor P) {i=[0]} (p [a []]),
    valid (HasAncestor P) {i=[0,0]} (p [div [a []]]),
    valid (HasAncestor P) {i=[0,0,0]} (p [div [div [a []]]]),
    error (HasAncestor P) {i=[]} (p []),
    error (HasAncestor P) {i=[0]} (div [a []]),
    error (HasAncestor P) {i=[0,0]} (div [div [a []]]),
    error (HasAncestor P) {i=[0,0,0]} (div [div [div [a []]]])
    )
hasAncestor = (trivial,trivial,trivial, trivial, trivial, trivial, trivial)

noAncestor: (
    error (NoAncestor P) {i=[0]} (p [a []]),
    error (NoAncestor P) {i=[0,0]} (p [div [a []]]),
    error (NoAncestor P) {i=[0,0,0]} (p [div [div [a []]]]),
    valid (NoAncestor P) {i=[]} (p []),
    valid (NoAncestor P) {i=[0]} (div [a []]),
    valid (NoAncestor P) {i=[0,0]} (div [div [a []]]),
    valid (NoAncestor P) {i=[0,0,0]} (div [div [div [a []]]])
    )
noAncestor = (trivial,trivial,trivial, trivial, trivial, trivial, trivial)

hasAncestorAttribute: (
    valid (HasAncestorAttribute P Id) {i=[0]} (p [id "", a []]),
    valid (HasAncestorAttribute P Id) {i=[0,0]} (p [id "", div [a []]]),
    valid (HasAncestorAttribute P Id) {i=[0,0,0]} (p [id "", div [div [a []]]]),
    error (HasAncestorAttribute P Id) {i=[0]} (p [a []]),
    error (HasAncestorAttribute P Id) {i=[0,0]} (p [div [a []]]),
    error (HasAncestorAttribute P Id) {i=[0,0,0]} (p [div [div [a []]]]),
    error (HasAncestorAttribute P Id) {i=[0]} (div [a []]),
    error (HasAncestorAttribute P Id) {i=[0,0]} (div [div [a []]]),
    error (HasAncestorAttribute P Id) {i=[0,0,0]} (div [div [div [a []]]])
    )
hasAncestorAttribute = (trivial,trivial,trivial, trivial, trivial, trivial, trivial, trivial, trivial)

hasDescendant: (
    valid (HasDescendant A) (p [ a []]),
    valid (HasDescendant A) (p [ p [], a []]),
    valid (HasDescendant A) (p [ p [], p [a []]]),
    valid (HasDescendant A) (p [ p [], p [p [], p [a []]]]),
    error (HasDescendant A) (p []),
    error (HasDescendant A) (p [ p [], p [p [], p [p []]]])
    )
hasDescendant = (trivial, trivial, trivial, trivial, trivial, trivial)

hasDescendantCategory: (
    valid (HasDescendantCategory ScriptSupporting) (p [ script []]),
    valid (HasDescendantCategory ScriptSupporting) (p [ p [], script []]),
    valid (HasDescendantCategory ScriptSupporting) (p [ p [], p [script []]]),
    valid (HasDescendantCategory ScriptSupporting) (p [ p [], p [p [], p [script []]]]),
    error (HasDescendantCategory ScriptSupporting) (p []),
    error (HasDescendantCategory ScriptSupporting) (p [ p [], p [p [], p [p []]]])
    )
hasDescendantCategory = (trivial, trivial, trivial, trivial, trivial, trivial)

noDescendant: (
    error (NoDescendant A) (p [ a []]),
    error (NoDescendant A) (p [ p [], a []]),
    error (NoDescendant A) (p [ p [], p [a []]]),
    error (NoDescendant A) (p [ p [], p [p [], p [a []]]]),
    valid (NoDescendant A) (p []),
    valid (NoDescendant A) (p [ p [], p [p [], p [p []]]])
    )
noDescendant = (trivial, trivial, trivial, trivial, trivial, trivial)

noDescendantCategory: (
    error (NoDescendantCategory ScriptSupporting) (p [ script []]),
    error (NoDescendantCategory ScriptSupporting) (p [ p [], script []]),
    error (NoDescendantCategory ScriptSupporting) (p [ p [], p [script []]]),
    error (NoDescendantCategory ScriptSupporting) (p [ p [], p [p [], p [script []]]]),
    valid (NoDescendantCategory ScriptSupporting) (p []),
    valid (NoDescendantCategory ScriptSupporting) (p [ p [], p [p [], p [p []]]])
    )
noDescendantCategory = (trivial, trivial, trivial, trivial, trivial, trivial)

noIndirectDescendant: (
    valid (NoIndirectDescendant A) (p [ a []]),
    valid (NoIndirectDescendant A) (p [ p [], a []]),
    error (NoIndirectDescendant A) (p [ p [], p [a []]]),
    error (NoIndirectDescendant A) (p [ p [], p [p [], p [a []]]]),
    valid (NoIndirectDescendant A) (p []),
    valid (NoIndirectDescendant A) (p [ p [], p [p [], p [p []]]])
    )
noIndirectDescendant = (trivial, trivial, trivial, trivial, trivial, trivial)

noDescendantAttribute: (
    error (NoDescendantAttribute Id) (p [ p [id ""]]),
    error (NoDescendantAttribute Id) (p [ p [], p [id ""]]),
    error (NoDescendantAttribute Id) (p [ p [], p [p [id ""]]]),
    error (NoDescendantAttribute Id) (p [ p [], p [p [], p [p [id ""]]]]),
    valid (NoDescendantAttribute Id) (p []),
    valid (NoDescendantAttribute Id) (p [id ""]),
    valid (NoDescendantAttribute Id) (p [ p [], p [p [], p [p []]]])
    )
noDescendantAttribute = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)


uniqueDescendantAttribute: (
    valid (UniqueDescendantAttribute A Id) (p [ a [id ""]]),
    valid (UniqueDescendantAttribute A Id) (p [ p [], a [id ""]]),
    valid (UniqueDescendantAttribute A Id) (p [ p [], p [p [], p [a [id ""]]]]),
    valid (UniqueDescendantAttribute A Id) (p []),
    valid (UniqueDescendantAttribute A Id) (p [id ""]),
    valid (UniqueDescendantAttribute A Id) (p [ p [], p [p [], p [p []]]]),
    error (UniqueDescendantAttribute A Id) (p [ a [id ""], a [id ""]]),
    error (UniqueDescendantAttribute A Id) (p [ a [id ""], p [], p [a [id ""]]])
    )
uniqueDescendantAttribute = (trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial)

uniqueDescendantCategory: (
    valid (UniqueDescendantCategory ScriptSupporting) (p [ script []]),
    valid (UniqueDescendantCategory ScriptSupporting) (p [ p [], script []]),
    valid (UniqueDescendantCategory ScriptSupporting) (p [ p [], p [p [], p [script []]]]),
    valid (UniqueDescendantCategory ScriptSupporting) (p []),
    valid (UniqueDescendantCategory ScriptSupporting) (p [id ""]),
    valid (UniqueDescendantCategory ScriptSupporting) (p [ p [], p [p [], p [p []]]]),
    error (UniqueDescendantCategory ScriptSupporting) (p [ script [], script []]),
    error (UniqueDescendantCategory ScriptSupporting) (p [ script [], p [], p [script []]])
    )
uniqueDescendantCategory = (trivial, trivial, trivial, trivial, trivial, trivial, trivial, trivial)

beforeURLs: (
    valid BeforeURLs {i=[0]} (head'' [base []]),
    valid BeforeURLs {i=[0]} (head'' [link [], base []]),
    error BeforeURLs {i=[1]} (head'' [link [href ""], base []]),
    error BeforeURLs {i=[1]} (head'' [div [link [href ""]], div [base []]])
    )
beforeURLs = (trivial,trivial,trivial, trivial)

uniqueCharset: (
    valid UniqueCharset (head'' [meta []]),
    valid UniqueCharset (head'' [meta [charset ""]]),
    error UniqueCharset (head'' [meta [charset ""], meta [], meta [charset ""]])
    )
uniqueCharset = (trivial, trivial, trivial)

uniqueTranslationPerLanguage: (
    valid UniqueTranslationPerLanguage (head'' [meta []]),
    valid UniqueTranslationPerLanguage (head'' [meta [name "application-name"]]),
    error UniqueTranslationPerLanguage (head'' [meta [name "application-name"], meta [name "application-name"]]),
    valid UniqueTranslationPerLanguage (head'' [meta [name "application-name"], meta [name "application-name", lang "en"]]),
    error UniqueTranslationPerLanguage (head'' [meta [name "application-name", lang "en"], meta [name "application-name", lang "en"]]),
    valid UniqueTranslationPerLanguage (head'' [meta [name "application-name", lang "en"], meta [name "application-name", lang "es"]])
    )
uniqueTranslationPerLanguage = (trivial, trivial, trivial, trivial, trivial, trivial)

uniqueDescription: (
    valid UniqueDescription (head'' [meta []]),
    valid UniqueDescription (head'' [meta [name "description"]]),
    error UniqueDescription (head'' [meta [name "description"], meta [], meta [name "description"]])
    )
uniqueDescription = (trivial, trivial, trivial)

uniqueThemeColorPerMedia: (
    valid UniqueThemeColorPerMedia (head'' [meta []]),
    valid UniqueThemeColorPerMedia (head'' [meta [name "theme-color"]]),
    error UniqueThemeColorPerMedia (head'' [meta [name "theme-color"], meta [name "theme-color"]]),
    valid UniqueThemeColorPerMedia (head'' [meta [name "theme-color"], meta [name "theme-color", media ""]]),
    error UniqueThemeColorPerMedia (head'' [meta [name "theme-color", media "width > 1000"], meta [name "theme-color", media "width > 1000"]]),
    valid UniqueThemeColorPerMedia (head'' [meta [name "theme-color", media "width > 1000"], meta [name "theme-color", media "width <= 1000"]])
    )
uniqueThemeColorPerMedia = (trivial, trivial, trivial, trivial, trivial, trivial)

uniqueColorScheme: (
    valid UniqueColorScheme (head'' [meta []]),
    valid UniqueColorScheme (head'' [meta [name "color-scheme"]]),
    error UniqueColorScheme (head'' [meta [name "color-scheme"], meta [], meta [name "color-scheme"]])
    )
uniqueColorScheme = (trivial, trivial, trivial)

uniqueEncoding: (
    valid UniqueEncoding (head'' [meta []]),
    valid UniqueEncoding (head'' [meta [name "charset"]]),
    valid UniqueEncoding (head'' [meta [httpEquiv "content-type"]]),
    error UniqueEncoding (head'' [meta [name "charset"], meta [], meta [name "charset"]]),
    error UniqueEncoding (head'' [meta [name "charset"], meta [], meta [httpEquiv "content-type"]]),
    error UniqueEncoding (head'' [meta [httpEquiv "content-type"], meta [], meta [httpEquiv "content-type"]]),
    error UniqueEncoding (head'' [meta [name "charset", httpEquiv "content-type"]])
    )
uniqueEncoding = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

uniqueMetaState: (
    valid UniqueMetaState (head'' [meta []]),
    valid UniqueMetaState (head'' [meta [httpEquiv "a"]]),
    valid UniqueMetaState (head'' [meta [httpEquiv "a"], meta [httpEquiv "b"]]),
    error UniqueMetaState (head'' [meta [httpEquiv "a"], meta [httpEquiv "a"]]),
    error UniqueMetaState (head'' [meta [httpEquiv "a"], meta [httpEquiv "A"]])
    )
uniqueMetaState = (trivial, trivial, trivial, trivial, trivial)

ifHeadingAtLeastOneH1 : (
    valid CorrectHeadingLevel (body [ h1 [] ]),
    error CorrectHeadingLevel (body [ h5 [] ]),
    error CorrectHeadingLevel (body [h1 [], section [h3 []]]),
    valid CorrectHeadingLevel (body [h1 [], section [h2 []]]),
    valid CorrectHeadingLevel (body [h1 [], section [ headingoffset "1", h1 []]]),
    error CorrectHeadingLevel (body [h1 [], section [ headingoffset "2", h2 []]]),
    valid CorrectHeadingLevel (body [h1 [], section [ headingoffset "2", section [headingreset "", h1 []]]])
    )
ifHeadingAtLeastOneH1 = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

uniqueTrackPerLangAndLabel: (
    valid UniqueTrackPerLangAndLabel (video []),
    valid UniqueTrackPerLangAndLabel (video [track []]),
    error UniqueTrackPerLangAndLabel (video [track [], track []]),
    valid UniqueTrackPerLangAndLabel (video [track [], track [kind "subtitles"]]),
    valid UniqueTrackPerLangAndLabel (video [track [], track [srclang "es"]]),
    valid UniqueTrackPerLangAndLabel (video [track [], track [label' "label"]]),
    error UniqueTrackPerLangAndLabel (video [
            track [kind "subtitles", srclang "es", label' "label"],
            track [kind "subtitles", srclang "es", label' "label"]
        ])
    )
uniqueTrackPerLangAndLabel = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)

hierarchicallyCorrectMain: (
    valid HierarchicallyCorrectMain {i=[0,0]} (html [body [main []]]),
    valid HierarchicallyCorrectMain {i=[0,0]} (form [div [main []]]),
    valid HierarchicallyCorrectMain {i=[0]} (custom "my-elem" [main []]),
    error HierarchicallyCorrectMain {i=[0,0]} (section [div [main []]])
    )
hierarchicallyCorrectMain = (trivial, trivial, trivial, trivial)

otherRadioGroupOptions: (
    error OtherRadioGroupOptions {i=[0]} (form [ input [type "radio", name "name"] ]),
    valid OtherRadioGroupOptions {i=[0]} (form [ input [type "radio", name "name"], input [type "radio", name "name"] ]),
    error OtherRadioGroupOptions {i=[0]} (form [ input [type "radio", name "name"], input [type "radio", name "name'"] ]),
    valid OtherRadioGroupOptions {i=[0]} (form [ input [type "radio"], input [type "radio", name "name"] ])
    )
otherRadioGroupOptions = (trivial, trivial, trivial, trivial)

hasPlaceholderLabelOption: (
    error HasPlaceholderLabelOption (select []),
    valid HasPlaceholderLabelOption (select [option []]),
    valid HasPlaceholderLabelOption (select [option [value ""]]),
    error HasPlaceholderLabelOption (select [option [value "a"]])
    )
hasPlaceholderLabelOption = (trivial, trivial, trivial, trivial)


uniqueOpenPerGroup: (
    valid UniqueOpenPerGroup {i=[0]} (div [details [open' ""]]),
    valid UniqueOpenPerGroup {i=[0]} (div [details [open' ""], details [open' ""]]),
    valid UniqueOpenPerGroup {i=[0]} (div [details [name "a", open' ""], details [open' ""]]),
    valid UniqueOpenPerGroup {i=[0]} (div [details [name "a", open' ""], details [name "b", open' ""]]),
    error UniqueOpenPerGroup {i=[0]} (div [details [name "a", open' ""], details [name "a", open' ""]]),
    valid UniqueOpenPerGroup {i=[0]} (div [details [name "a", open' ""], details [name "a"]])
    )
uniqueOpenPerGroup = (trivial, trivial, trivial, trivial, trivial, trivial)

notNestedSameName: (
    valid NotNestedSameName {i=[0,0]} (details [div [details []]]),
    valid NotNestedSameName {i=[0,0]} (details [name "a", div [details []]]),
    valid NotNestedSameName {i=[0,0]} (details [div [details [name "a"]]]),
    valid NotNestedSameName {i=[0,0]} (details [name "a", div [details [name "b"]]]),
    error NotNestedSameName {i=[0,0]} (details [name "a", div [details [name "a"]]])
    )
notNestedSameName = (trivial, trivial, trivial, trivial, trivial)

correctTableModel: (
    valid CorrectTableModel (table []),
    valid CorrectTableModel (table [tr [td [], td []]]),
    error CorrectTableModel (table [colgroup [span' "3"]]),
    error CorrectTableModel (table [colgroup [span' "3"], tr [td[], td []]]),
    error CorrectTableModel (table [colgroup [span' "3"], tr [td[colspan "2"]]]),
    error CorrectTableModel (table [colgroup [col [span' "1"], col [span' "2"]], tr [td[], td []]]),
    error CorrectTableModel (table [tr [td [], td [rowspan "2"]], tr [td [colspan "2"]]])
    )
correctTableModel = (trivial, trivial, trivial, trivial, trivial, trivial, trivial)