||| Collection of proofs about content model parsin

import DSL
import Implementation

parses model trees result = run (parser model) (map fromTree trees) = Accept () (map fromTree result)
doesntParse model trees = run (parser model) (map fromTree trees) = Reject

tag: (
    parses (Tag P) [p []] [],
    parses (Tag P) [p [], p []] [p []],
    parses (Tag P) [p [], h1 []] [h1 []],
    doesntParse (Tag P) [],
    doesntParse (Tag P) [head'' [], p []]
    )
tag = (Refl, Refl, Refl, Refl, Refl)

category: (
    parses (Category Heading) [h1 []] [],
    parses (Category Heading) [h1 [], p []] [p []],
    doesntParse (Category Heading) [],
    doesntParse (Category Heading) [p [], h1 []]
    )
category = (Refl, Refl, Refl, Refl)

nothing: (
    parses Nothing [] [],
    parses Nothing [comment "c",comment "c'"] [],
    doesntParse Nothing [p []],
    parses Nothing [text "", comment "c'", text ""] [],
    doesntParse Nothing [text "prueba"]
    )
nothing = (Refl, Refl, Refl, Refl, Refl)

text: (
    parses Text [text "a"] [],
    parses Text [p []] [p []]
    )
text = (Refl, Refl)

optional: (
    parses (Optional (Tag P)) [] [],
    parses (Optional (Tag P)) [p []] [],
    parses (Optional (Tag P)) [p [], h1 []] [h1 []],
    parses (Optional (Tag P)) [h1 []] [h1 []],
    parses (Optional (Sequence [Tag P, Tag H1])) [p [], h1 [], p []] [p []]
    )
optional = (Refl,Refl, Refl, Refl, Refl)

many: (
    parses (Many (Tag P)) [] [],
    parses (Many (Tag P)) [p []] [],
    parses (Many (Tag P)) [p [], p []] [],
    parses (Many (Tag P)) [p [], p [], h1 []] [h1 []],
    parses (Many (Sequence [Tag P, Tag H1])) [p [], h1 [], p [], h1 [], p []] [p []]
    )
many = (Refl, Refl, Refl, Refl, Refl )

atLeastOne: (
    doesntParse (AtLeastOne (Tag P)) [],
    doesntParse (AtLeastOne (Tag P)) [h1 []],
    parses (AtLeastOne (Tag P)) [p [], p []] [],
    parses (AtLeastOne (Tag P)) [p [], h1 []] [h1 []],
    parses (AtLeastOne (Sequence [Tag P, Tag H1])) [p [], h1 [], p []] [p []]
    )
atLeastOne = (Refl,Refl, Refl, Refl, Refl)

intermixed: (
    parses (Intermixed Heading (Tag P)) [p []] [],
    parses (Intermixed Heading (Tag P)) [h1 [], p [], h2 [], h3 []] [],
    doesntParse (Intermixed Heading (Tag P)) [h1 [], h2 [], h3 []],
    parses (Intermixed Heading (Many (Tag P))) [h1 [], p [], p [], h2 [], p [], h3 []] []
    )
intermixed = (Refl, Refl, Refl, Refl)

sequence: (
    parses (Sequence [Tag P]) [p []] [],
    parses (Sequence [Tag P, Tag H1, Tag P]) [p [], h1 [], p []] [],
    doesntParse (Sequence [Tag P, Tag H1]) [p [], p []],
    doesntParse (Sequence [Tag P, Nothing]) [p [], h1 []],
    parses (Sequence [Tag P, Nothing]) [p []] [],
    parses (Sequence [Tag P, Many (Tag H1), Tag P]) [p [], p []] [],
    parses (Sequence [Tag P, Many (Tag H1), Tag P]) [p [], h1 [], h1 [], p []] []
    )
sequence = (Refl, Refl, Refl, Refl, Refl, Refl, Refl)

any: (
    doesntParse (Any []) [],
    doesntParse (Any []) [p []],
    parses (Any [Tag P]) [p []] [],
    parses (Any [Tag P]) [p [], h1 []] [h1 []],
    parses (Any [Tag P, Tag H1]) [h1 []] [],
    parses (Any [Tag P, Tag H1]) [p []] [],
    parses (Any [Sequence [Tag H1, Tag P], Tag H1]) [h1 [], p []] [],
    parses (Many (Any [Tag P, Tag H1])) [h1 [], p [], p []] []
    )
any = (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl)


transparentReplacing: (
    replaceTransparent [] === [],
    replaceTransparent [fromTree (p [])] === [fromTree $ p []],
    replaceTransparent [fromTree (a [])] === [],
    replaceTransparent [fromTree (a [a [], a []])] === [],
    replaceTransparent [fromTree (a [p [], h1 []])] === children (fromTree (a [p [], h1 []]))
    )
transparentReplacing = (Refl, Refl, Refl, Refl, Refl)

transparent: (
    parses (Tag P) [a [p []]] [],
    doesntParse (Sequence [Tag P, Tag P]) [a [p [], h1 []]],
    parses (Sequence [Tag P, Tag P]) [p [], a [p []]] [],
    parses (Sequence [Tag P, Tag P]) [video [src "", track [], track [], p [], p []]] []
    )
transparent = (Refl, Refl, Refl, Refl)