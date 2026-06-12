||| Collection of types for working with HTML specifications
module Core.Specification

import Core.Condition
import Core.Tags
import Data.String
import Core.Attributes


namespace Category
    ||| One of an HTML element's categories
    ||| Categories can be fixed or they can depend on whether the element meets a condition
    public export
    data ElementCategory =
          (>>>) Category -- Unconditional
        | (==>) Condition Category -- Conditional

||| Pattern that an element's children must conform to
||| See: https://html.spec.whatwg.org/multipage/dom.html#content-models
public export
data ContentModel =
      Anything -- Any pattern is valid
    | Nothing -- Only comments and empty text
    | Text -- Only comments and text
    | Tag Tag -- A specific tag
    | Category Category -- A tag that belongs to a specific category

    -- QUANTIFIERS
    | Optional ContentModel -- 0 or 1 of a model
    | Many ContentModel -- 0 or more of a model
    | AtLeastOne ContentModel -- 1 or more of a model
    | Intermixed Category ContentModel -- A model, but elements from a given category are ignored 

    -- Combinators
    | Sequence (List ContentModel) -- All the models, one after the other
    | Any (List ContentModel) -- Any of the models
    | Transparent ContentModel -- For content model checking purposes, a transparent element is replaced with its children

public export
data ElementContentModel =
      (>>>) ContentModel 
    | When (List (Condition, ContentModel)) ContentModel

-- The requirements an HTML element must belong to
public export
record Specification where
    constructor Spec
    categories: List ElementCategory -- Categories it belongs to
    contentModel: ElementContentModel -- Model its content must follow
    {default [] attributes: List Attribute} -- Allowed attributes (other than global attributes)
    {default [] restrictions: Condition} -- Miscellaneous restrictions