||| Collection of types for working with HTML trees
module Core.Tags

||| HTML tag
||| Tags are divided according to the section of the HTML Live Specification
||| they are introduced in 
public export
data Tag =
      -- Metadata
        Html | Head | Title | Base | Link | Meta | Style
      -- Sectioning
      | Body | Article | Section | Nav | Aside | H1 | H2 | H3 | H4 | H5 | H6 | Hgroup | Header | Footer | Address
      -- Grouping
      | P | Hr | Pre | Blockquote | Ol | Ul | Menu | Li | Dl | Dt | Dd | Figure | Figcaption | Main | Search | Div
      -- Text semantics
      | A | Em | Strong | Small | S | Cite | Q | Dfn | Abbr | Ruby | Rt | Rp | Data | Time | Code | Var | Samp | Kbd | Sub | Sup | I | B | U | Mark | Bdi | Bdo | Span | Br | Wbr
      -- Edits
      | Ins | Del
      -- Embedded Content
      | Picture | Source | Img | Iframe | Embed | Object | Video | Audio | Track | Map | Area | Math | SVG
      -- Tabular
      | Table | Caption | Colgroup | Col | Tbody | Thead | Tfoot | Tr | Td | Th
      -- Forms
      | Form | Label | Input | Button | Select | Datalist | Optgroup | Option | Textarea | Output | Progress | Meter | Fieldset | Legend | SelectedContent
      -- Interactive
      | Details | Summary | Dialog
      
      -- Scripting
      | Script | Noscript | Template | Slot | Canvas

      -- Custom elements
      | Custom String

public export
Eq Tag where
      Html == Html = True
      Head == Head = True
      Title == Title = True
      Base == Base = True
      Link == Link = True
      Meta == Meta = True
      Style == Style = True
      Body == Body  = True
      Article == Article  = True
      Section == Section  = True
      Nav == Nav  = True
      Aside == Aside  = True
      H1 == H1 = True
      H2 == H2 = True
      H3 == H3 = True
      H4 == H4 = True
      H5 == H5 = True
      H6 == H6 = True
      Hgroup == Hgroup = True
      Header == Header = True
      Footer == Footer = True
      Address == Address = True
      P == P = True 
      Hr == Hr = True 
      Pre == Pre = True 
      Blockquote == Blockquote = True 
      Ol == Ol = True 
      Ul == Ul = True 
      Menu == Menu = True 
      Li == Li = True 
      Dl == Dl = True 
      Dt == Dt = True 
      Dd == Dd = True 
      Figure == Figure = True 
      Figcaption == Figcaption = True 
      Main == Main = True 
      Search == Search = True 
      Div == Div = True
      A == A = True
      Em == Em = True
      Strong == Strong = True
      Small == Small = True
      S == S = True
      Cite == Cite = True
      Q == Q = True
      Dfn == Dfn = True
      Abbr == Abbr = True
      Ruby == Ruby = True
      Rt == Rt = True
      Rp == Rp = True
      Data == Data = True
      Time == Time = True
      Code == Code = True
      Var == Var = True
      Samp == Samp = True
      Kbd == Kbd = True
      Sub == Sub = True
      Sup == Sup = True
      I == I = True
      B == B = True
      U == U = True
      Mark == Mark = True
      Bdi == Bdi = True
      Bdo == Bdo = True
      Span == Span = True
      Br == Br = True
      Wbr == Wbr = True
      Ins == Ins = True
      Del == Del = True
      Picture == Picture = True
      Source == Source = True
      Img == Img = True
      Iframe == Iframe = True
      Embed == Embed = True
      Object == Object = True
      Video == Video = True
      Audio == Audio = True
      Track == Track = True
      Map == Map = True
      Area == Area = True
      Math == Math = True
      SVG == SVG = True
      Table == Table = True
      Caption == Caption = True
      Colgroup == Colgroup = True 
      Col == Col = True 
      Tbody == Tbody = True 
      Thead == Thead = True 
      Tfoot == Tfoot = True 
      Tr == Tr = True 
      Td == Td = True 
      Th == Th = True
      Script == Script = True
      Template == Template = True
      Form == Form = True
      Label == Label = True
      Input == Input = True
      Button == Button = True
      Select == Select = True
      Datalist == Datalist = True
      Optgroup == Optgroup = True
      Option == Option = True
      Textarea == Textarea = True
      Output == Output = True
      Progress == Progress = True
      Meter == Meter = True
      Fieldset == Fieldset = True
      Legend == Legend = True
      Details == Details = True
      Summary == Summary = True
      Dialog == Dialog = True
      SelectedContent == SelectedContent = True
      Custom str == Custom str' = str == str'
      _ == _ = False


public export
Show Tag where
    show Html = "html"
    show Head = "head" 
    show Title = "title" 
    show Base = "base" 
    show Link = "link" 
    show Meta = "meta" 
    show Style = "style" 
    show Body = "body"
    show Article = "article"
    show Section = "section"
    show Nav = "nav"
    show Aside = "aside"
    show H1 = "h1"
    show H2 = "h2"
    show H3 = "h3"
    show H4 = "h4"
    show H5 = "h5"
    show H6 = "h6"
    show Hgroup = "hgroup"
    show Header = "header"
    show Footer = "footer"
    show Address = "address"
    show P = "p"
    show Hr = "hr"
    show Pre = "pre"
    show Blockquote = "blockquote"
    show Ol = "ol"
    show Ul = "ul"
    show Menu = "menu"
    show Li = "li"
    show Dl = "dl"
    show Dt = "dt"
    show Dd = "dd"
    show Figure = "figure"
    show Figcaption = "figcaption"
    show Main = "main"
    show Search = "search"
    show Div = "div"
    show A = "a"
    show Em = "em"
    show Strong = "strong"
    show Small = "small"
    show S = "s"
    show Cite = "cite"
    show Q = "q"
    show Dfn = "dfn"
    show Abbr = "abbr"
    show Ruby = "ruby"
    show Rt = "rt"
    show Rp = "rp"
    show Data = "data"
    show Time = "time"
    show Code = "code"
    show Var = "var"
    show Samp = "samp"
    show Kbd = "kbd"
    show Sub = "sub"
    show Sup = "sup"
    show I = "i"
    show B = "b"
    show U = "u"
    show Mark = "mark"
    show Bdi = "bdi"
    show Bdo = "bdo"
    show Span = "span"
    show Br = "br"
    show Wbr = "wbr"
    show Ins = "ins"
    show Del = "del"
    show Picture = "picture"
    show Source = "source"
    show Img = "img"
    show Iframe = "iframe"
    show Embed = "embed"
    show Object = "object"
    show Video = "video"
    show Audio = "audio"
    show Track = "track"
    show Map = "map"
    show Area = "area"
    show Math = "math"
    show SVG= "svg"
    show Table = "table"
    show Caption = "caption"
    show Colgroup = "colgroup"
    show Col = "col"
    show Tbody = "tbody"
    show Thead = "thead"
    show Tfoot = "tfoot"
    show Tr = "tr"
    show Td = "td"
    show Th = "th"
    show Form = "form"
    show Label = "label"
    show Input = "input"
    show Button = "button"
    show Select = "select"
    show Datalist = "datalist"
    show Optgroup = "optgroup"
    show Option = "option"
    show Textarea = "textarea"
    show Output = "output"
    show Progress = "progress"
    show Meter = "meter"
    show Fieldset = "fieldset"
    show Legend = "legend"
    show SelectedContent = "selectedcontent"
    show Details = "details"
    show Summary = "summary"
    show Dialog = "dialog"
    show Script = "script"
    show Noscript = "noscript"
    show Template = "template"
    show Slot = "slot"
    show Canvas = "canvas"
    show (Custom str) = str