||| Specification for the elements in section 4.10
||| https://html.spec.whatwg.org/multipage/forms.html
||| https://html.spec.whatwg.org/multipage/input.html
||| https://html.spec.whatwg.org/multipage/form-elements.html
||| https://html.spec.whatwg.org/multipage/form-control-infrastructure.html
||| https://html.spec.whatwg.org/multipage/interactive-elements.html
module Definitions.Forms

import Core.Tags
import Core.Specification
import Core.Condition
import Core.Attributes


public export
specification : Tag -> Maybe Specification

specification Form = Just <| Spec {
    categories = [ >>> Flow, >>> Palpable],
    contentModel = >>> Many (Category Flow),
    attributes = [ AcceptCharset, Action, Autocomplete, Enctype, Method, Name, Novalidate, Target, Rel],
    restrictions = [
        AcceptCharset `Is` "UTF-8",
        NotEmpty Name,
        Autocomplete `Is` "on" || "off",
        Action `Is` URL,
        Enctype `Is` "application/x-www-form-urlencoded" || "multipart/form-data" || "text/plain",
        Method `Is` "get" || "post" || "dialog",
        Novalidate `Is` Boolean,
        Target `Is` NavigableTargetName || NavigationKeyword,
        Rel `Is` List " " ("noreferrer" || "noopener" || "opener"),

        UniqueTag Name,
        NoDescendant Form
    ]
}

specification Label = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing, >>> Interactive, >>> Palpable],
    contentModel = >>> Many (Category Phrasing),
    attributes = [For],
    restrictions = [
        Has For ==> NoDescendantCategory Labelable,
        HasNot For ==> UniqueDescendantCategory Labelable,
        NoDescendant Label,
        For `References` Id
    ]
}

specification Input = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,

        >>> Submitable,
        >>> Resettable,
        >>> AutocapitalizeAndAutocorrectInheriting,
        >>> FormAssociated,
        Type' `IsNot` "hidden" ==> Interactive,
        Type' `IsNot` "hidden" ==> Labelable,
        Type' `IsNot` "hidden" ==> Palpable
    ],

    contentModel = >>> Nothing,

    attributes = [ Accept, Alpha, Alt, Autocomplete, Checked, Colorspace, Dirname, Disabled, Form, Formaction, Formenctype, Formmethod, Formnovalidate, Formtarget, Height, List, Max, Maxlength, Min, Minlength, Multiple, Name, Pattern, Placeholder, Popovertarget, Popovertargetaction, Readonly, Required, Size, Src, Step, Type', Value, Width],
    
    restrictions = [
        Autocomplete `Is` Autocomplete,
        (Autocomplete `Is` "on" || "off") ==> HasNot Hidden,

        Checked `Is` Boolean,
        NotEmpty Dirname,
        Disabled `Is` Boolean,
        Form `ReferencesTag` (Form,Name),
        NotEmpty Name,
        Name `IsNot` "isIndex",
        List `ReferencesTag` (Datalist, Id),
        Maxlength `Is` NonNegative,
        Minlength `Is` NonNegative,
        Minlength <= Maxlength,
        Pattern `Is` Pattern,
        Placeholder `Is` SingleLine,
        Popovertarget `ReferencesAttribute` (Popover,Id),
        Popovertargetaction `Is` "toggle" || "show" || "hide",
        Readonly `Is` Boolean,
        Required `Is` Boolean,
        Size `Is` Positive,
        Type' `Is` "text" || "number" || "hidden" || "search" || "tel" || "url" ||
                   "email" || "password" || "date" || "month" || "week" || "time" ||
                   "datetime" || "Integer'" || "range" || "color" || "checkbox" ||
                   "radio" || "file" || "submit" || "image" || "reset" || "button",

        Type' `IsRequired` "hidden" ==> Name `IsRequired` "_charset_" ==> HasNot Value,
        Type' `IsRequired` "hidden" ==> HasNot Accept,
        Type' `IsRequired` "hidden" ==> HasNot Alpha,
        Type' `IsRequired` "hidden" ==> HasNot Alt,
        Type' `IsRequired` "hidden" ==> HasNot Checked,
        Type' `IsRequired` "hidden" ==> HasNot Colorspace,
        Type' `IsRequired` "hidden" ==> HasNot Formaction,
        Type' `IsRequired` "hidden" ==> HasNot Formenctype,
        Type' `IsRequired` "hidden" ==> HasNot Formmethod,
        Type' `IsRequired` "hidden" ==> HasNot Formnovalidate,
        Type' `IsRequired` "hidden" ==> HasNot Formtarget,
        Type' `IsRequired` "hidden" ==> HasNot Height,
        Type' `IsRequired` "hidden" ==> HasNot List,
        Type' `IsRequired` "hidden" ==> HasNot Max,
        Type' `IsRequired` "hidden" ==> HasNot Maxlength,
        Type' `IsRequired` "hidden" ==> HasNot Min,
        Type' `IsRequired` "hidden" ==> HasNot Minlength,
        Type' `IsRequired` "hidden" ==> HasNot Multiple,
        Type' `IsRequired` "hidden" ==> HasNot Pattern,
        Type' `IsRequired` "hidden" ==> HasNot Placeholder,
        Type' `IsRequired` "hidden" ==> HasNot Popovertarget,
        Type' `IsRequired` "hidden" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "hidden" ==> HasNot Readonly,
        Type' `IsRequired` "hidden" ==> HasNot Required,
        Type' `IsRequired` "hidden" ==> HasNot Size,
        Type' `IsRequired` "hidden" ==> HasNot Src,
        Type' `IsRequired` "hidden" ==> HasNot Step,
        Type' `IsRequired` "hidden" ==> HasNot Width,

        Type' `IsRequired` "text" ==> Value `Is` SingleLine,
        Type' `IsRequired` "text" ==> HasNot Accept,
        Type' `IsRequired` "text" ==> HasNot Alpha,
        Type' `IsRequired` "text" ==> HasNot Alt,
        Type' `IsRequired` "text" ==> HasNot Checked,
        Type' `IsRequired` "text" ==> HasNot Colorspace,
        Type' `IsRequired` "text" ==> HasNot Formaction,
        Type' `IsRequired` "text" ==> HasNot Formenctype,
        Type' `IsRequired` "text" ==> HasNot Formmethod,
        Type' `IsRequired` "text" ==> HasNot Formnovalidate,
        Type' `IsRequired` "text" ==> HasNot Formtarget,
        Type' `IsRequired` "text" ==> HasNot Height,
        Type' `IsRequired` "text" ==> HasNot Max,
        Type' `IsRequired` "text" ==> HasNot Min,
        Type' `IsRequired` "text" ==> HasNot Multiple,
        Type' `IsRequired` "text" ==> HasNot Popovertarget,
        Type' `IsRequired` "text" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "text" ==> HasNot Src,
        Type' `IsRequired` "text" ==> HasNot Step,
        Type' `IsRequired` "text" ==> HasNot Width,

        Type' `IsRequired` "search" ==> Value `Is` SingleLine,
        Type' `IsRequired` "search" ==> HasNot Accept,
        Type' `IsRequired` "search" ==> HasNot Alpha,
        Type' `IsRequired` "search" ==> HasNot Alt,
        Type' `IsRequired` "search" ==> HasNot Checked,
        Type' `IsRequired` "search" ==> HasNot Colorspace,
        Type' `IsRequired` "search" ==> HasNot Formaction,
        Type' `IsRequired` "search" ==> HasNot Formenctype,
        Type' `IsRequired` "search" ==> HasNot Formmethod,
        Type' `IsRequired` "search" ==> HasNot Formnovalidate,
        Type' `IsRequired` "search" ==> HasNot Formtarget,
        Type' `IsRequired` "search" ==> HasNot Height,
        Type' `IsRequired` "search" ==> HasNot Max,
        Type' `IsRequired` "search" ==> HasNot Min,
        Type' `IsRequired` "search" ==> HasNot Multiple,
        Type' `IsRequired` "search" ==> HasNot Popovertarget,
        Type' `IsRequired` "search" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "search" ==> HasNot Src,
        Type' `IsRequired` "search" ==> HasNot Step,
        Type' `IsRequired` "search" ==> HasNot Width,
        
        Type' `IsRequired` "tel" ==> Value `Is` SingleLine,
        Type' `IsRequired` "tel" ==> HasNot Accept,
        Type' `IsRequired` "tel" ==> HasNot Alpha,
        Type' `IsRequired` "tel" ==> HasNot Alt,
        Type' `IsRequired` "tel" ==> HasNot Checked,
        Type' `IsRequired` "tel" ==> HasNot Colorspace,
        Type' `IsRequired` "tel" ==> HasNot Formaction,
        Type' `IsRequired` "tel" ==> HasNot Formenctype,
        Type' `IsRequired` "tel" ==> HasNot Formmethod,
        Type' `IsRequired` "tel" ==> HasNot Formnovalidate,
        Type' `IsRequired` "tel" ==> HasNot Formtarget,
        Type' `IsRequired` "tel" ==> HasNot Height,
        Type' `IsRequired` "tel" ==> HasNot Max,
        Type' `IsRequired` "tel" ==> HasNot Min,
        Type' `IsRequired` "tel" ==> HasNot Multiple,
        Type' `IsRequired` "tel" ==> HasNot Popovertarget,
        Type' `IsRequired` "tel" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "tel" ==> HasNot Src,
        Type' `IsRequired` "tel" ==> HasNot Step,
        Type' `IsRequired` "tel" ==> HasNot Width,
        
        Type' `IsRequired` "url" ==> Value `Is` AbsoluteURL,
        Type' `IsRequired` "url" ==> HasNot Accept,
        Type' `IsRequired` "url" ==> HasNot Alpha,
        Type' `IsRequired` "url" ==> HasNot Alt,
        Type' `IsRequired` "url" ==> HasNot Checked,
        Type' `IsRequired` "url" ==> HasNot Colorspace,
        Type' `IsRequired` "url" ==> HasNot Formaction,
        Type' `IsRequired` "url" ==> HasNot Formenctype,
        Type' `IsRequired` "url" ==> HasNot Formmethod,
        Type' `IsRequired` "url" ==> HasNot Formnovalidate,
        Type' `IsRequired` "url" ==> HasNot Formtarget,
        Type' `IsRequired` "url" ==> HasNot Height,
        Type' `IsRequired` "url" ==> HasNot Max,
        Type' `IsRequired` "url" ==> HasNot Min,
        Type' `IsRequired` "url" ==> HasNot Multiple,
        Type' `IsRequired` "url" ==> HasNot Popovertarget,
        Type' `IsRequired` "url" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "url" ==> HasNot Src,
        Type' `IsRequired` "url" ==> HasNot Step,
        Type' `IsRequired` "url" ==> HasNot Width,
        
        [Type' `IsRequired` "email", HasNot Multiple] ==> Value `Is` Email,
        [Type' `IsRequired` "email", Has Multiple] ==> Value `Is` List "," Email,
        Type' `IsRequired` "email" ==> HasNot Accept,
        Type' `IsRequired` "email" ==> HasNot Alpha,
        Type' `IsRequired` "email" ==> HasNot Alt,
        Type' `IsRequired` "email" ==> HasNot Checked,
        Type' `IsRequired` "email" ==> HasNot Colorspace,
        Type' `IsRequired` "email" ==> HasNot Formaction,
        Type' `IsRequired` "email" ==> HasNot Formenctype,
        Type' `IsRequired` "email" ==> HasNot Formmethod,
        Type' `IsRequired` "email" ==> HasNot Formnovalidate,
        Type' `IsRequired` "email" ==> HasNot Formtarget,
        Type' `IsRequired` "email" ==> HasNot Height,
        Type' `IsRequired` "email" ==> HasNot Max,
        Type' `IsRequired` "email" ==> HasNot Min,
        Type' `IsRequired` "email" ==> HasNot Popovertarget,
        Type' `IsRequired` "email" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "email" ==> HasNot Src,
        Type' `IsRequired` "email" ==> HasNot Step,
        Type' `IsRequired` "email" ==> HasNot Width,
        
        Type' `IsRequired` "password" ==> Value `Is` SingleLine,
        Type' `IsRequired` "password" ==> HasNot Accept,
        Type' `IsRequired` "password" ==> HasNot Alpha,
        Type' `IsRequired` "password" ==> HasNot Alt,
        Type' `IsRequired` "password" ==> HasNot Checked,
        Type' `IsRequired` "password" ==> HasNot Colorspace,
        Type' `IsRequired` "password" ==> HasNot Formaction,
        Type' `IsRequired` "password" ==> HasNot Formenctype,
        Type' `IsRequired` "password" ==> HasNot Formmethod,
        Type' `IsRequired` "password" ==> HasNot Formnovalidate,
        Type' `IsRequired` "password" ==> HasNot Formtarget,
        Type' `IsRequired` "password" ==> HasNot Height,
        Type' `IsRequired` "password" ==> HasNot List,
        Type' `IsRequired` "password" ==> HasNot Max,
        Type' `IsRequired` "password" ==> HasNot Min,
        Type' `IsRequired` "password" ==> HasNot Multiple,
        Type' `IsRequired` "password" ==> HasNot Popovertarget,
        Type' `IsRequired` "password" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "password" ==> HasNot Src,
        Type' `IsRequired` "password" ==> HasNot Step,
        Type' `IsRequired` "password" ==> HasNot Width,

        Type' `IsRequired` "date" ==> Value `Is` Date || "",
        Type' `IsRequired` "date" ==> Min `Is` Date,
        Type' `IsRequired` "date" ==> Max `Is` Date,
        Type' `IsRequired` "date" ==> Min <= Max,
        Type' `IsRequired` "date" ==> Step `Is` "any" || NonNegative,
        Type' `IsRequired` "date" ==> HasNot Accept,
        Type' `IsRequired` "date" ==> HasNot Alpha,
        Type' `IsRequired` "date" ==> HasNot Alt,
        Type' `IsRequired` "date" ==> HasNot Checked,
        Type' `IsRequired` "date" ==> HasNot Colorspace,
        Type' `IsRequired` "date" ==> HasNot Dirname,
        Type' `IsRequired` "date" ==> HasNot Formaction,
        Type' `IsRequired` "date" ==> HasNot Formenctype,
        Type' `IsRequired` "date" ==> HasNot Formmethod,
        Type' `IsRequired` "date" ==> HasNot Formnovalidate,
        Type' `IsRequired` "date" ==> HasNot Formtarget,
        Type' `IsRequired` "date" ==> HasNot Height,
        Type' `IsRequired` "date" ==> HasNot Maxlength,
        Type' `IsRequired` "date" ==> HasNot Minlength,
        Type' `IsRequired` "date" ==> HasNot Multiple,
        Type' `IsRequired` "date" ==> HasNot Pattern,
        Type' `IsRequired` "date" ==> HasNot Placeholder,
        Type' `IsRequired` "date" ==> HasNot Popovertarget,
        Type' `IsRequired` "date" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "date" ==> HasNot Size,
        Type' `IsRequired` "date" ==> HasNot Src,
        Type' `IsRequired` "date" ==> HasNot Width,
        
        Type' `IsRequired` "month" ==> Value `Is` Month || "",
        Type' `IsRequired` "month" ==> Min `Is` Month,
        Type' `IsRequired` "month" ==> Max `Is` Month,
        Type' `IsRequired` "month" ==> Min <= Max,
        Type' `IsRequired` "month" ==> Step `Is` "any" || NonNegative,
        Type' `IsRequired` "month" ==> HasNot Accept,
        Type' `IsRequired` "month" ==> HasNot Alpha,
        Type' `IsRequired` "month" ==> HasNot Alt,
        Type' `IsRequired` "month" ==> HasNot Checked,
        Type' `IsRequired` "month" ==> HasNot Colorspace,
        Type' `IsRequired` "month" ==> HasNot Dirname,
        Type' `IsRequired` "month" ==> HasNot Formaction,
        Type' `IsRequired` "month" ==> HasNot Formenctype,
        Type' `IsRequired` "month" ==> HasNot Formmethod,
        Type' `IsRequired` "month" ==> HasNot Formnovalidate,
        Type' `IsRequired` "month" ==> HasNot Formtarget,
        Type' `IsRequired` "month" ==> HasNot Height,
        Type' `IsRequired` "month" ==> HasNot Maxlength,
        Type' `IsRequired` "month" ==> HasNot Minlength,
        Type' `IsRequired` "month" ==> HasNot Multiple,
        Type' `IsRequired` "month" ==> HasNot Pattern,
        Type' `IsRequired` "month" ==> HasNot Placeholder,
        Type' `IsRequired` "month" ==> HasNot Popovertarget,
        Type' `IsRequired` "month" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "month" ==> HasNot Size,
        Type' `IsRequired` "month" ==> HasNot Src,
        Type' `IsRequired` "month" ==> HasNot Width,

        Type' `IsRequired` "week" ==> Value `Is` Week || "",
        Type' `IsRequired` "week" ==> Min `Is` Week,
        Type' `IsRequired` "week" ==> Max `Is` Week,
        Type' `IsRequired` "week" ==> Min <= Max,
        Type' `IsRequired` "week" ==> Step `Is` "any" || NonNegative,
        Type' `IsRequired` "week" ==> HasNot Accept,
        Type' `IsRequired` "week" ==> HasNot Alpha,
        Type' `IsRequired` "week" ==> HasNot Alt,
        Type' `IsRequired` "week" ==> HasNot Checked,
        Type' `IsRequired` "week" ==> HasNot Colorspace,
        Type' `IsRequired` "week" ==> HasNot Dirname,
        Type' `IsRequired` "week" ==> HasNot Formaction,
        Type' `IsRequired` "week" ==> HasNot Formenctype,
        Type' `IsRequired` "week" ==> HasNot Formmethod,
        Type' `IsRequired` "week" ==> HasNot Formnovalidate,
        Type' `IsRequired` "week" ==> HasNot Formtarget,
        Type' `IsRequired` "week" ==> HasNot Height,
        Type' `IsRequired` "week" ==> HasNot Maxlength,
        Type' `IsRequired` "week" ==> HasNot Minlength,
        Type' `IsRequired` "week" ==> HasNot Multiple,
        Type' `IsRequired` "week" ==> HasNot Pattern,
        Type' `IsRequired` "week" ==> HasNot Placeholder,
        Type' `IsRequired` "week" ==> HasNot Popovertarget,
        Type' `IsRequired` "week" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "week" ==> HasNot Size,
        Type' `IsRequired` "week" ==> HasNot Src,
        Type' `IsRequired` "week" ==> HasNot Width,

        Type' `IsRequired` "time" ==> Value `Is` Time || "",
        Type' `IsRequired` "time" ==> Min `Is` Time,
        Type' `IsRequired` "time" ==> Max `Is` Time,
        Type' `IsRequired` "time" ==> Step `Is` "any" || NonNegative,
        Type' `IsRequired` "time" ==> HasNot Accept,
        Type' `IsRequired` "time" ==> HasNot Alpha,
        Type' `IsRequired` "time" ==> HasNot Alt,
        Type' `IsRequired` "time" ==> HasNot Checked,
        Type' `IsRequired` "time" ==> HasNot Colorspace,
        Type' `IsRequired` "time" ==> HasNot Dirname,
        Type' `IsRequired` "time" ==> HasNot Formaction,
        Type' `IsRequired` "time" ==> HasNot Formenctype,
        Type' `IsRequired` "time" ==> HasNot Formmethod,
        Type' `IsRequired` "time" ==> HasNot Formnovalidate,
        Type' `IsRequired` "time" ==> HasNot Formtarget,
        Type' `IsRequired` "time" ==> HasNot Height,
        Type' `IsRequired` "time" ==> HasNot Maxlength,
        Type' `IsRequired` "time" ==> HasNot Minlength,
        Type' `IsRequired` "time" ==> HasNot Multiple,
        Type' `IsRequired` "time" ==> HasNot Pattern,
        Type' `IsRequired` "time" ==> HasNot Placeholder,
        Type' `IsRequired` "time" ==> HasNot Popovertarget,
        Type' `IsRequired` "time" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "time" ==> HasNot Size,
        Type' `IsRequired` "time" ==> HasNot Src,
        Type' `IsRequired` "time" ==> HasNot Width,

        
        Type' `IsRequired` "datetime-local" ==> Value `Is` Datetime || "",
        Type' `IsRequired` "datetime-local" ==> Min `Is` Datetime,
        Type' `IsRequired` "datetime-local" ==> Max `Is` Datetime,
        Type' `IsRequired` "datetime-local" ==> Min <= Max,
        Type' `IsRequired` "datetime-local" ==> Step `Is` "any" || NonNegative,
        Type' `IsRequired` "datetime-local" ==> HasNot Accept,
        Type' `IsRequired` "datetime-local" ==> HasNot Alpha,
        Type' `IsRequired` "datetime-local" ==> HasNot Alt,
        Type' `IsRequired` "datetime-local" ==> HasNot Checked,
        Type' `IsRequired` "datetime-local" ==> HasNot Colorspace,
        Type' `IsRequired` "datetime-local" ==> HasNot Dirname,
        Type' `IsRequired` "datetime-local" ==> HasNot Formaction,
        Type' `IsRequired` "datetime-local" ==> HasNot Formenctype,
        Type' `IsRequired` "datetime-local" ==> HasNot Formmethod,
        Type' `IsRequired` "datetime-local" ==> HasNot Formnovalidate,
        Type' `IsRequired` "datetime-local" ==> HasNot Formtarget,
        Type' `IsRequired` "datetime-local" ==> HasNot Height,
        Type' `IsRequired` "datetime-local" ==> HasNot Maxlength,
        Type' `IsRequired` "datetime-local" ==> HasNot Minlength,
        Type' `IsRequired` "datetime-local" ==> HasNot Multiple,
        Type' `IsRequired` "datetime-local" ==> HasNot Pattern,
        Type' `IsRequired` "datetime-local" ==> HasNot Placeholder,
        Type' `IsRequired` "datetime-local" ==> HasNot Popovertarget,
        Type' `IsRequired` "datetime-local" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "datetime-local" ==> HasNot Size,
        Type' `IsRequired` "datetime-local" ==> HasNot Src,
        Type' `IsRequired` "datetime-local" ==> HasNot Width,
        
        Type' `IsRequired` "range" ==> Value `Is` Float || "",
        Type' `IsRequired` "range" ==> Min `Is` Float,
        Type' `IsRequired` "range" ==> Max `Is` Float,
        Type' `IsRequired` "range" ==> Min <= Max,
        Type' `IsRequired` "range" ==> Step `Is` Float || "",
        Type' `IsRequired` "range" ==> ((Min `IsRequired` Integer') || (Value `Is` Integer')) ==> Step `Is` "any" || Integer',
        Type' `IsRequired` "range" ==> HasNot Accept,
        Type' `IsRequired` "range" ==> HasNot Alpha,
        Type' `IsRequired` "range" ==> HasNot Alt,
        Type' `IsRequired` "range" ==> HasNot Checked,
        Type' `IsRequired` "range" ==> HasNot Colorspace,
        Type' `IsRequired` "range" ==> HasNot Dirname,
        Type' `IsRequired` "range" ==> HasNot Formaction,
        Type' `IsRequired` "range" ==> HasNot Formenctype,
        Type' `IsRequired` "range" ==> HasNot Formmethod,
        Type' `IsRequired` "range" ==> HasNot Formnovalidate,
        Type' `IsRequired` "range" ==> HasNot Formtarget,
        Type' `IsRequired` "range" ==> HasNot Height,
        Type' `IsRequired` "range" ==> HasNot Maxlength,
        Type' `IsRequired` "range" ==> HasNot Minlength,
        Type' `IsRequired` "range" ==> HasNot Multiple,
        Type' `IsRequired` "range" ==> HasNot Pattern,
        Type' `IsRequired` "range" ==> HasNot Placeholder,
        Type' `IsRequired` "range" ==> HasNot Popovertarget,
        Type' `IsRequired` "range" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "range" ==> HasNot Size,
        Type' `IsRequired` "range" ==> HasNot Src,
        Type' `IsRequired` "range" ==> HasNot Width,
        
        Type' `IsRequired` "color" ==> Value `Is` Color || "",
        Type' `IsRequired` "color" ==> Alpha `Is` Boolean,
        Type' `IsRequired` "color" ==> Colorspace `Is` "limited-srgb" || "display-p3",
        Type' `IsRequired` "color" ==> HasNot Accept,
        Type' `IsRequired` "color" ==> HasNot Alt,
        Type' `IsRequired` "color" ==> HasNot Checked,
        Type' `IsRequired` "color" ==> HasNot Dirname,
        Type' `IsRequired` "color" ==> HasNot Formaction,
        Type' `IsRequired` "color" ==> HasNot Formenctype,
        Type' `IsRequired` "color" ==> HasNot Formmethod,
        Type' `IsRequired` "color" ==> HasNot Formnovalidate,
        Type' `IsRequired` "color" ==> HasNot Formtarget,
        Type' `IsRequired` "color" ==> HasNot Height,
        Type' `IsRequired` "color" ==> HasNot Max,
        Type' `IsRequired` "color" ==> HasNot Maxlength,
        Type' `IsRequired` "color" ==> HasNot Min,
        Type' `IsRequired` "color" ==> HasNot Minlength,
        Type' `IsRequired` "color" ==> HasNot Multiple,
        Type' `IsRequired` "color" ==> HasNot Pattern,
        Type' `IsRequired` "color" ==> HasNot Placeholder,
        Type' `IsRequired` "color" ==> HasNot Popovertarget,
        Type' `IsRequired` "color" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "color" ==> HasNot Readonly,
        Type' `IsRequired` "color" ==> HasNot Required,
        Type' `IsRequired` "color" ==> HasNot Size,
        Type' `IsRequired` "color" ==> HasNot Src,
        Type' `IsRequired` "color" ==> HasNot Step,
        Type' `IsRequired` "color" ==> HasNot Width,

        Type' `IsRequired` "radio" ==> OtherRadioGroupOptions,
        Type' `IsRequired` "radio" ==> HasNot Accept,
        Type' `IsRequired` "radio" ==> HasNot Alpha,
        Type' `IsRequired` "radio" ==> HasNot Alt,
        Type' `IsRequired` "radio" ==> HasNot Autocomplete,
        Type' `IsRequired` "radio" ==> HasNot Colorspace,
        Type' `IsRequired` "radio" ==> HasNot Dirname,
        Type' `IsRequired` "radio" ==> HasNot Formaction,
        Type' `IsRequired` "radio" ==> HasNot Formenctype,
        Type' `IsRequired` "radio" ==> HasNot Formmethod,
        Type' `IsRequired` "radio" ==> HasNot Formnovalidate,
        Type' `IsRequired` "radio" ==> HasNot Formtarget,
        Type' `IsRequired` "radio" ==> HasNot Height,
        Type' `IsRequired` "radio" ==> HasNot List,
        Type' `IsRequired` "radio" ==> HasNot Max,
        Type' `IsRequired` "radio" ==> HasNot Maxlength,
        Type' `IsRequired` "radio" ==> HasNot Min,
        Type' `IsRequired` "radio" ==> HasNot Minlength,
        Type' `IsRequired` "radio" ==> HasNot Multiple,
        Type' `IsRequired` "radio" ==> HasNot Pattern,
        Type' `IsRequired` "radio" ==> HasNot Placeholder,
        Type' `IsRequired` "radio" ==> HasNot Popovertarget,
        Type' `IsRequired` "radio" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "radio" ==> HasNot Readonly,
        Type' `IsRequired` "radio" ==> HasNot Size,
        Type' `IsRequired` "radio" ==> HasNot Src,
        Type' `IsRequired` "radio" ==> HasNot Step,
        Type' `IsRequired` "radio" ==> HasNot Width,

        Type' `IsRequired` "checkbox" ==> HasNot Accept,
        Type' `IsRequired` "checkbox" ==> HasNot Alpha,
        Type' `IsRequired` "checkbox" ==> HasNot Alt,
        Type' `IsRequired` "checkbox" ==> HasNot Autocomplete,
        Type' `IsRequired` "checkbox" ==> HasNot Colorspace,
        Type' `IsRequired` "checkbox" ==> HasNot Dirname,
        Type' `IsRequired` "checkbox" ==> HasNot Formaction,
        Type' `IsRequired` "checkbox" ==> HasNot Formenctype,
        Type' `IsRequired` "checkbox" ==> HasNot Formmethod,
        Type' `IsRequired` "checkbox" ==> HasNot Formnovalidate,
        Type' `IsRequired` "checkbox" ==> HasNot Formtarget,
        Type' `IsRequired` "checkbox" ==> HasNot Height,
        Type' `IsRequired` "checkbox" ==> HasNot List,
        Type' `IsRequired` "checkbox" ==> HasNot Max,
        Type' `IsRequired` "checkbox" ==> HasNot Maxlength,
        Type' `IsRequired` "checkbox" ==> HasNot Min,
        Type' `IsRequired` "checkbox" ==> HasNot Minlength,
        Type' `IsRequired` "checkbox" ==> HasNot Multiple,
        Type' `IsRequired` "checkbox" ==> HasNot Pattern,
        Type' `IsRequired` "checkbox" ==> HasNot Placeholder,
        Type' `IsRequired` "checkbox" ==> HasNot Popovertarget,
        Type' `IsRequired` "checkbox" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "checkbox" ==> HasNot Readonly,
        Type' `IsRequired` "checkbox" ==> HasNot Size,
        Type' `IsRequired` "checkbox" ==> HasNot Src,
        Type' `IsRequired` "checkbox" ==> HasNot Step,
        Type' `IsRequired` "checkbox" ==> HasNot Width,

        
        Type' `IsRequired` "file" ==> Accept `Is` Set "," FileAccept,
        Type' `IsRequired` "file" ==> HasNot Value,
        Type' `IsRequired` "file" ==> HasNot Alpha,
        Type' `IsRequired` "file" ==> HasNot Alt,
        Type' `IsRequired` "file" ==> HasNot Autocomplete,
        Type' `IsRequired` "file" ==> HasNot Checked,
        Type' `IsRequired` "file" ==> HasNot Colorspace,
        Type' `IsRequired` "file" ==> HasNot Dirname,
        Type' `IsRequired` "file" ==> HasNot Formaction,
        Type' `IsRequired` "file" ==> HasNot Formenctype,
        Type' `IsRequired` "file" ==> HasNot Formmethod,
        Type' `IsRequired` "file" ==> HasNot Formnovalidate,
        Type' `IsRequired` "file" ==> HasNot Formtarget,
        Type' `IsRequired` "file" ==> HasNot Height,
        Type' `IsRequired` "file" ==> HasNot List,
        Type' `IsRequired` "file" ==> HasNot Max,
        Type' `IsRequired` "file" ==> HasNot Maxlength,
        Type' `IsRequired` "file" ==> HasNot Min,
        Type' `IsRequired` "file" ==> HasNot Minlength,
        Type' `IsRequired` "file" ==> HasNot Pattern,
        Type' `IsRequired` "file" ==> HasNot Popovertarget,
        Type' `IsRequired` "file" ==> HasNot Popovertargetaction,
        Type' `IsRequired` "file" ==> HasNot Placeholder,
        Type' `IsRequired` "file" ==> HasNot Readonly,
        Type' `IsRequired` "file" ==> HasNot Size,
        Type' `IsRequired` "file" ==> HasNot Src,
        Type' `IsRequired` "file" ==> HasNot Step,
        Type' `IsRequired` "file" ==> HasNot Width,

        
        Type' `IsRequired` "submit" ==> Formaction `Is` URL,
        Type' `IsRequired` "submit" ==> Formenctype `Is` "application/x-www-form-urlencoded" || "multipart/form-data" || "text/plain",
        Type' `IsRequired` "submit" ==> Formmethod `Is` "get" || "post" || "dialog",
        Type' `IsRequired` "submit" ==> Formnovalidate `Is` Boolean,
        Type' `IsRequired` "submit" ==> Formtarget `Is` NavigableTargetName || NavigationKeyword,
        Type' `IsRequired` "submit" ==> HasNot Accept,
        Type' `IsRequired` "submit" ==> HasNot Alpha,
        Type' `IsRequired` "submit" ==> HasNot Alt,
        Type' `IsRequired` "submit" ==> HasNot Autocomplete,
        Type' `IsRequired` "submit" ==> HasNot Checked,
        Type' `IsRequired` "submit" ==> HasNot Colorspace,
        Type' `IsRequired` "submit" ==> HasNot Height,
        Type' `IsRequired` "submit" ==> HasNot List,
        Type' `IsRequired` "submit" ==> HasNot Max,
        Type' `IsRequired` "submit" ==> HasNot Maxlength,
        Type' `IsRequired` "submit" ==> HasNot Min,
        Type' `IsRequired` "submit" ==> HasNot Minlength,
        Type' `IsRequired` "submit" ==> HasNot Multiple,
        Type' `IsRequired` "submit" ==> HasNot Pattern,
        Type' `IsRequired` "submit" ==> HasNot Placeholder,
        Type' `IsRequired` "submit" ==> HasNot Readonly,
        Type' `IsRequired` "submit" ==> HasNot Required,
        Type' `IsRequired` "submit" ==> HasNot Size,
        Type' `IsRequired` "submit" ==> HasNot Src,
        Type' `IsRequired` "submit" ==> HasNot Step,
        Type' `IsRequired` "submit" ==> HasNot Width,

        Type' `IsRequired` "image" ==> Src `Is` URL,
        Type' `IsRequired` "image" ==> Has Alt,
        Type' `IsRequired` "image" ==> NotEmpty Alt,
        Type' `IsRequired` "image" ==> Width `Is` NonNegative,
        Type' `IsRequired` "image" ==> Height `Is` NonNegative,
        Type' `IsRequired` "image" ==> Formaction `Is` URL,
        Type' `IsRequired` "image" ==> Formenctype `Is` "application/x-www-form-urlencoded" || "multipart/form-data" || "text/plain",
        Type' `IsRequired` "image" ==> Formmethod `Is` "get" || "post" || "dialog",
        Type' `IsRequired` "image" ==> Formnovalidate `Is` Boolean,
        Type' `IsRequired` "image" ==> Formtarget `Is` NavigableTargetName || NavigationKeyword,
        Type' `IsRequired` "image" ==> HasNot Accept,
        Type' `IsRequired` "image" ==> HasNot Alpha,
        Type' `IsRequired` "image" ==> HasNot Autocomplete,
        Type' `IsRequired` "image" ==> HasNot Checked,
        Type' `IsRequired` "image" ==> HasNot Colorspace,
        Type' `IsRequired` "image" ==> HasNot Dirname,
        Type' `IsRequired` "image" ==> HasNot List,
        Type' `IsRequired` "image" ==> HasNot Max,
        Type' `IsRequired` "image" ==> HasNot Maxlength,
        Type' `IsRequired` "image" ==> HasNot Min,
        Type' `IsRequired` "image" ==> HasNot Minlength,
        Type' `IsRequired` "image" ==> HasNot Multiple,
        Type' `IsRequired` "image" ==> HasNot Pattern,
        Type' `IsRequired` "image" ==> HasNot Placeholder,
        Type' `IsRequired` "image" ==> HasNot Readonly,
        Type' `IsRequired` "image" ==> HasNot Required,
        Type' `IsRequired` "image" ==> HasNot Size,
        Type' `IsRequired` "image" ==> HasNot Step,

        Type' `IsRequired` "reset" ==> HasNot Accept,
        Type' `IsRequired` "reset" ==> HasNot Alpha,
        Type' `IsRequired` "reset" ==> HasNot Alt,
        Type' `IsRequired` "reset" ==> HasNot Autocomplete,
        Type' `IsRequired` "reset" ==> HasNot Checked,
        Type' `IsRequired` "reset" ==> HasNot Colorspace,
        Type' `IsRequired` "reset" ==> HasNot Dirname,
        Type' `IsRequired` "reset" ==> HasNot Formaction,
        Type' `IsRequired` "reset" ==> HasNot Formenctype,
        Type' `IsRequired` "reset" ==> HasNot Formmethod,
        Type' `IsRequired` "reset" ==> HasNot Formnovalidate,
        Type' `IsRequired` "reset" ==> HasNot Formtarget,
        Type' `IsRequired` "reset" ==> HasNot Height,
        Type' `IsRequired` "reset" ==> HasNot List,
        Type' `IsRequired` "reset" ==> HasNot Max,
        Type' `IsRequired` "reset" ==> HasNot Maxlength,
        Type' `IsRequired` "reset" ==> HasNot Min,
        Type' `IsRequired` "reset" ==> HasNot Minlength,
        Type' `IsRequired` "reset" ==> HasNot Multiple,
        Type' `IsRequired` "reset" ==> HasNot Pattern,
        Type' `IsRequired` "reset" ==> HasNot Placeholder,
        Type' `IsRequired` "reset" ==> HasNot Readonly,
        Type' `IsRequired` "reset" ==> HasNot Required,
        Type' `IsRequired` "reset" ==> HasNot Size,
        Type' `IsRequired` "reset" ==> HasNot Src,
        Type' `IsRequired` "reset" ==> HasNot Step,
        Type' `IsRequired` "reset" ==> HasNot Width,

        
        Type' `IsRequired` "button" ==> HasNot Accept,
        Type' `IsRequired` "button" ==> HasNot Alpha,
        Type' `IsRequired` "button" ==> HasNot Alt,
        Type' `IsRequired` "button" ==> HasNot Autocomplete,
        Type' `IsRequired` "button" ==> HasNot Checked,
        Type' `IsRequired` "button" ==> HasNot Colorspace,
        Type' `IsRequired` "button" ==> HasNot Dirname,
        Type' `IsRequired` "button" ==> HasNot Formaction,
        Type' `IsRequired` "button" ==> HasNot Formenctype,
        Type' `IsRequired` "button" ==> HasNot Formmethod,
        Type' `IsRequired` "button" ==> HasNot Formnovalidate,
        Type' `IsRequired` "button" ==> HasNot Formtarget,
        Type' `IsRequired` "button" ==> HasNot Height,
        Type' `IsRequired` "button" ==> HasNot List,
        Type' `IsRequired` "button" ==> HasNot Max,
        Type' `IsRequired` "button" ==> HasNot Maxlength,
        Type' `IsRequired` "button" ==> HasNot Min,
        Type' `IsRequired` "button" ==> HasNot Minlength,
        Type' `IsRequired` "button" ==> HasNot Multiple,
        Type' `IsRequired` "button" ==> HasNot Pattern,
        Type' `IsRequired` "button" ==> HasNot Placeholder,
        Type' `IsRequired` "button" ==> HasNot Readonly,
        Type' `IsRequired` "button" ==> HasNot Required,
        Type' `IsRequired` "button" ==> HasNot Size,
        Type' `IsRequired` "button" ==> HasNot Src,
        Type' `IsRequired` "button" ==> HasNot Step,
        Type' `IsRequired` "button" ==> HasNot Width
    ]
}

specification Button = Just <| Spec {
    categories = [
        >>> Flow, >>> Phrasing, >>> Interactive, >>> Palpable,
        >>> Listed, >>> Labelable, >>> Submitable, >>> AutocapitalizeAndAutocorrectInheriting, >>> FormAssociated
    ],

    contentModel = When [
        (HasParent Select,
            Sequence [Many (Category Phrasing), Optional (Tag SelectedContent), Many (Category Phrasing)]
        )
    ] (Many (Category Phrasing)),

    attributes = [
        Command, Commandfor, Disabled, Form, Formaction, Formenctype, Formmethod, Formnovalidate, Formtarget, Name, Popovertarget, Popovertargetaction, Type', Value
    ],

    restrictions = [
        Type' `Is` "submit" || "reset" || "button",

        Command `Is` "toggle-popover" || "show-popover" || "hide-popover" || "request-close" || "show-modal" || "close" || Prefix "--",

        Disabled `Is` Boolean,
        
        Commandfor `References` Id,
        Formaction `Is` URL,
        Formenctype `Is` "application/x-www-form-urlencoded" || "multipart/form-data" || "text/plain",
        Formmethod `Is` "get" || "post" || "dialog",
        Formnovalidate `Is` Boolean,
        Formtarget `Is` NavigableTargetName || NavigationKeyword,
        
        Form `ReferencesTag` (Form,Name),

        Has Formaction ==> (Type' `Is` "submit") || [HasNone [Type', Command, Commandfor], NotParent Select],
        Has Formenctype ==> (Type' `Is` "submit") || [HasNone [Type', Command, Commandfor], NotParent Select],
        Has Formmethod ==> (Type' `Is` "submit") || [HasNone [Type', Command, Commandfor], NotParent Select],
        Has Formnovalidate ==> (Type' `Is` "submit") || [HasNone [Type', Command, Commandfor], NotParent Select],
        Has Formtarget ==> (Type' `Is` "submit") || [HasNone [Type', Command, Commandfor], NotParent Select],

        Popovertarget `ReferencesAttribute` (Popover,Id),
        Popovertargetaction `Is` "toggle" || "show" || "hide"
    ]    
}

specification Select = Just <| Spec {
    categories = [
        >>> Flow, >>> Phrasing, >>> Interactive, >>> Palpable,
        >>> Listed, >>> Labelable, >>> Submitable, >>> Resettable, >>> AutocapitalizeAndAutocorrectInheriting, >>> FormAssociated
    ],

    contentModel = When [
        (HasNot Multiple, 
            Sequence [Optional (Tag Button),Many (Category SelectElementInnerContent)]
        ),
        ([Has Multiple, Size `IsRequired` "1"], 
            Sequence [Optional (Tag Button),Many (Category SelectElementInnerContent)]
        )
    ] (Many (Category SelectElementInnerContent)),

    attributes = [
        Autocomplete,
        Disabled,
        Form,
        Multiple,
        Name,
        Required,
        Size
    ],

    restrictions = [
        Autocomplete `Is` Autocomplete,
        Disabled `Is` Boolean,
        Multiple `Is` Boolean,
        NotEmpty Name,
        Name `IsNot` "isIndex",
        Required `Is` Boolean,
        Size `Is` Positive,

        Form `ReferencesTag` (Form,Name),

        [Has Required, HasNot Multiple, Size `Is` "1"] ==> HasPlaceholderLabelOption,

        HasNot Multiple ==> UniqueDescendantAttribute Option Selected
    ]
}

specification Datalist = Just <| Spec {
    categories = [ >>> Flow, >>> Phrasing ],
    contentModel = >>> Any [
        Many (Any [
            Tag Option,
            Category ScriptSupporting
        ]),
        Many (Category Phrasing)
    ]
}

specification Optgroup = Just <| Spec {
    categories = [ >>> SelectElementInnerContent ],
    contentModel = >>> Sequence [
        Optional (Tag Legend),
        Many (Category OptgroupElementInnerContent)
    ],
    attributes = [Disabled, Label],
    restrictions = [
        Disabled `Is` Boolean,
        NoChild Legend ==> Has Label
    ]
}

specification Option = Just <| Spec {
    categories = [
        >>> SelectElementInnerContent,
        >>> OptgroupElementInnerContent
    ],
    contentModel = When [
        ([Has Label, Has Value], Nothing),
        ([Has Label, HasNot Value], Text),
        ([HasNot Label, NoAncestor Datalist], Many (Any [Category OptionElementInnerContent, Category Phrasing]))
    ] Text,
    attributes = [Disabled, Label, Selected, Value],
    restrictions = [
        Disabled `Is` Boolean,
        NotEmpty Label,
        Selected `Is` Boolean,
        Value `Is` Anything
    ]
}

specification Textarea = Just <| Spec {
    categories = [
        >>> Flow, >>> Phrasing, >>> Interactive,
        >>> Listed, >>> Labelable, >>> Submitable, >>> Resettable, >>> AutocapitalizeAndAutocorrectInheriting, >>> FormAssociated,
        >>> Palpable
    ],

    contentModel = >>> Text,

    attributes = [
        Autocomplete, Cols, Dirname, Disabled, Form, Maxlength, Minlength, Name, Placeholder, Readonly, Required, Rows, Wrap
    ],

    restrictions = [
        Readonly `Is` Boolean,

        Cols `Is` Positive,
        Rows `Is` Positive,
        
        Wrap `Is` "soft" || "hard",
        Wrap `IsRequired` "hard" ==> Has Cols,
        
        Maxlength `Is` NonNegative,
        Minlength `Is` NonNegative,
        Minlength <= Maxlength,

        Required `Is` Boolean,
        Placeholder `Is` Anything,
        Disabled `Is` Boolean,
        NotEmpty Name,
        Name `IsNot` "isIndex",
        NotEmpty Dirname,
        Form `ReferencesTag` (Form,Name),
        Autocomplete `Is` Autocomplete
    ]
}

specification Output = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Phrasing,
        >>> Listed, >>> Labelable, >>> Resettable, >>> AutocapitalizeAndAutocorrectInheriting, >>> FormAssociated,
        >>> Palpable
    ],

    contentModel = >>> Many (Category Phrasing),

    attributes = [For, Form, Name],

    restrictions = [
        For `Is` Set " " Anything,
        EachReferencesId For,
        Form `ReferencesTag` (Form,Name), 
        NotEmpty Name,
        Name `IsNot` "isIndex"
    ]
}

specification Progress = Just <| Spec {
    categories = [
        >>> Flow, >>> Phrasing, >>> Labelable, >>> Palpable
    ],

    contentModel = >>> Many (Category Phrasing),

    attributes = [Value, Max],

    restrictions = [
        NoDescendant Progress,
        Value `Is` Float,
        Max `Is` Float,
        Value >= 1,
        Value <= Max,
        HasNot Max ==> Value <= 1
    ]
}

specification Meter = Just <| Spec {
    categories = [
        >>> Flow, >>> Phrasing, >>> Labelable, >>> Palpable
    ],

    contentModel = >>> Many (Category Phrasing),

    attributes = [Value, Max, Min, Low, High, Optimum],

    restrictions = [
        NoDescendant Progress,
        Value `Is` Float,
        Min `Is` Float,
        Max `Is` Float,
        Low `Is` Float,
        High `Is` Float,
        Optimum `Is` Float,


        Value >= 0,
        Min <= Max,
        HasNot Min ==> Max >= 0,
        HasNot Max ==> Min <= 1,

        Value <= Max,

        Has Low ==> Min <= Low,
        [HasNot Min, Has Low] ==> 0 <= Low,

        Has Low ==> Low <= Max,
        [HasNot Max, Has Low] ==> Low <= 1,

        Has High ==> Min <= High,
        [HasNot Min, Has High] ==> 0 <= High,

        Has High ==> High <= Max,
        [HasNot Max, Has High] ==> High <= 1,

        Has Optimum ==> Min <= Optimum,
        [HasNot Min, Has Optimum] ==> 0 <= Optimum,

        Has Optimum ==> Optimum <= High,
        [HasNot Max, Has Optimum] ==> Optimum <= 1,

        Low <= High
    ]
}

specification Fieldset = Just <| Spec {
    categories = [
        >>> Flow,
        >>> Listed, >>> AutocapitalizeAndAutocorrectInheriting, >>> FormAssociated,
        >>> Palpable
    ],

    contentModel = >>> Sequence [
        Optional (Tag Legend),
        Many (Category Flow)
    ],

    attributes = [Disabled, Form, Name],

    restrictions = [
        Disabled `Is` Boolean,
        Form `ReferencesTag` (Form,Name),
        NotEmpty Name,
        Name `IsNot` "isIndex"
    ]
}

specification Legend = Just <| Spec {
    categories = [],

    contentModel = When [
        (HasParent Optgroup, Many (Category Phrasing))
    ] (Intermixed Heading <| Many (Category Phrasing)),

    restrictions = [
        HasParent Optgroup ==> NoDescendantCategory Interactive,
        HasParent Optgroup ==> NoDescendantAttribute Tabindex
    ]
}

specification SelectedContent = Just <| Spec {
    categories = [ >>> Phrasing ],
    contentModel = >>> Nothing
}

specification _ = Nothing