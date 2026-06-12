||| Collection of types for working with HTML trees
module Core.Tags

import Derive.Prelude
import Deriving.Show

%language ElabReflection

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

%runElab derive "Tag" [Eq]

||| Derived implementation of Show. It keeps the same casing and doesn't
||| give out the correct string for custom tags
public export
showDefault : Show Tag
showDefault = %runElab derive

public export
Show Tag where
      show (Custom str) = str
      show tag = toLower (show @{showDefault} tag)