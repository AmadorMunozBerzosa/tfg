||| Specification for a given HTML element
||| It must be a standalone module to avoid cyclic dependencies
module Definitions.Specification

import Util.List

import Core.Tags
import Core.Attributes
import Core.Specification
import Core.Condition

import Definitions.DocumentMetadata
import Definitions.Sections
import Definitions.GroupingContent
import Definitions.TextSemantics
import Definitions.Edits
import Definitions.EmbeddedContent
import Definitions.TabularData
import Definitions.Forms
import Definitions.InteractiveElements
import Definitions.Scripting


-- Specification for a custom element tag
public export
autonomousCustomElement : Specification
autonomousCustomElement = Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Palpable,
        >>> Listed, >>> Labelable, >>> Submitable, >>> Resettable, >>> FormAssociated
    ],
    contentModel = >>> Transparent Nothing,
    attributes = [Form, Disabled, Readonly, Name],
    restrictions = [
        ValidTagName,
        HasNot Is,
        Readonly `Is` Boolean,
        Disabled `Is` Boolean,
        Form `ReferencesTag` (Form,Name),
        NotEmpty Name,
        Name `IsNot` "isIndex"
    ]
}

-- HTML specification for a given element tag
public export
specification : Tag -> Specification
specification tag =
    tryUntilJust tag [
        DocumentMetadata.specification,
        Sections.specification,
        GroupingContent.specification,
        TextSemantics.specification,
        Edits.specification,
        EmbeddedContent.specification,
        TabularData.specification,
        Forms.specification,
        InteractiveElements.specification,
        Scripting.specification
    ] autonomousCustomElement