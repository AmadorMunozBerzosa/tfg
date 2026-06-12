module DSL

import Implementation

||| The contents of an element are a mixture of attribute and children nodes
public export
ElementInfo : Type
ElementInfo = CastList (Either (Attribute,String) Node)

public export
Cast Node (Either (Attribute,String) Node) where
    cast = Right

public export
Cast (Attribute,String) (Either (Attribute,String) Node) where
    cast = Left

||| We allow consumers to use strings in lieu of text nodes
public export
Cast String (Either (Attribute,String) Node) where
    cast = Right . Leaf . Text

||| Defines an element node with the given tag
public export
createTag: Tag -> ElementInfo -> Node
createTag tag info =
    let (attributes,children) = classify (toList info) in
    
    Branch (Element tag (fromList attributes)) children

||| Defines a text node
public export
text : String -> Node
text a = Leaf (Text a)

||| Defines a comment node
public export
comment : String -> Node
comment a = Leaf (Comment a)

||| Defines a function for creating every element
namespace Elem
    public export
    html: ElementInfo -> Node
    html = createTag Html
    public export
    head'': ElementInfo -> Node
    head'' = createTag Head
    public export
    title: ElementInfo -> Node
    title = createTag Title
    public export
    base: ElementInfo -> Node
    base = createTag Base
    public export
    link: ElementInfo -> Node
    link = createTag Link
    public export
    meta: ElementInfo -> Node
    meta = createTag Meta
    public export
    style: ElementInfo -> Node
    style = createTag Style
    public export
    body: ElementInfo -> Node
    body = createTag Body
    public export
    article: ElementInfo -> Node
    article = createTag Article
    public export
    section: ElementInfo -> Node
    section = createTag Section
    public export
    nav: ElementInfo -> Node
    nav = createTag Nav
    public export
    aside: ElementInfo -> Node
    aside = createTag Aside
    public export
    h1: ElementInfo -> Node
    h1 = createTag H1
    public export
    h2: ElementInfo -> Node
    h2 = createTag H2
    public export
    h3: ElementInfo -> Node
    h3 = createTag H3
    public export
    h4: ElementInfo -> Node
    h4 = createTag H4
    public export
    h5: ElementInfo -> Node
    h5 = createTag H5
    public export
    h6: ElementInfo -> Node
    h6 = createTag H6
    public export
    hgroup: ElementInfo -> Node
    hgroup = createTag Hgroup
    public export
    header: ElementInfo -> Node
    header = createTag Header
    public export
    footer: ElementInfo -> Node
    footer = createTag Footer
    public export
    address: ElementInfo -> Node
    address = createTag Address
    public export
    p: ElementInfo -> Node
    p = createTag P
    public export
    hr: ElementInfo -> Node
    hr = createTag Hr
    public export
    pre: ElementInfo -> Node
    pre = createTag Pre
    public export
    blockquote: ElementInfo -> Node
    blockquote = createTag Blockquote
    public export
    ol: ElementInfo -> Node
    ol = createTag Ol
    public export
    ul: ElementInfo -> Node
    ul = createTag Ul
    public export
    menu: ElementInfo -> Node
    menu = createTag Menu
    public export
    li: ElementInfo -> Node
    li = createTag Li
    public export
    dl: ElementInfo -> Node
    dl = createTag Dl
    public export
    dt: ElementInfo -> Node
    dt = createTag Dt
    public export
    dd: ElementInfo -> Node
    dd = createTag Dd
    public export
    figure: ElementInfo -> Node
    figure = createTag Figure
    public export
    figcaption: ElementInfo -> Node
    figcaption = createTag Figcaption
    public export
    main: ElementInfo -> Node
    main = createTag Main
    public export
    search: ElementInfo -> Node
    search = createTag Search
    public export
    div: ElementInfo -> Node
    div = createTag Div
    public export
    a: ElementInfo -> Node
    a = createTag A
    public export
    em: ElementInfo -> Node
    em = createTag Em
    public export
    strong: ElementInfo -> Node
    strong = createTag Strong
    public export
    small: ElementInfo -> Node
    small = createTag Small
    public export
    s: ElementInfo -> Node
    s = createTag S
    public export
    cite: ElementInfo -> Node
    cite = createTag Cite
    public export
    q: ElementInfo -> Node
    q = createTag Q
    public export
    dfn: ElementInfo -> Node
    dfn = createTag Dfn
    public export
    abbr: ElementInfo -> Node
    abbr = createTag Abbr
    public export
    ruby: ElementInfo -> Node
    ruby = createTag Ruby
    public export
    rt: ElementInfo -> Node
    rt = createTag Rt
    public export
    rp: ElementInfo -> Node
    rp = createTag Rp
    public export
    data': ElementInfo -> Node
    data' = createTag Data
    public export
    time: ElementInfo -> Node
    time = createTag Time
    public export
    code: ElementInfo -> Node
    code = createTag Code
    public export
    var: ElementInfo -> Node
    var = createTag Var
    public export
    samp: ElementInfo -> Node
    samp = createTag Samp
    public export
    kbd: ElementInfo -> Node
    kbd = createTag Kbd
    public export
    sub: ElementInfo -> Node
    sub = createTag Sub
    public export
    sup: ElementInfo -> Node
    sup = createTag Sup
    public export
    i: ElementInfo -> Node
    i = createTag I
    public export
    b: ElementInfo -> Node
    b = createTag B
    public export
    u: ElementInfo -> Node
    u = createTag U
    public export
    mark: ElementInfo -> Node
    mark = createTag Mark
    public export
    bdi: ElementInfo -> Node
    bdi = createTag Bdi
    public export
    bdo: ElementInfo -> Node
    bdo = createTag Bdo
    public export
    span: ElementInfo -> Node
    span = createTag Span
    public export
    br: ElementInfo -> Node
    br = createTag Br
    public export
    wbr: ElementInfo -> Node
    wbr = createTag Wbr
    public export
    ins: ElementInfo -> Node
    ins = createTag Ins
    public export
    del: ElementInfo -> Node
    del = createTag Del
    public export
    picture: ElementInfo -> Node
    picture = createTag Picture
    public export
    source: ElementInfo -> Node
    source = createTag Source
    public export
    img: ElementInfo -> Node
    img = createTag Img
    public export
    iframe: ElementInfo -> Node
    iframe = createTag Iframe
    public export
    embed: ElementInfo -> Node
    embed = createTag Embed
    public export
    object: ElementInfo -> Node
    object = createTag Object
    public export
    video: ElementInfo -> Node
    video = createTag Video
    public export
    audio: ElementInfo -> Node
    audio = createTag Audio
    public export
    track: ElementInfo -> Node
    track = createTag Track
    public export
    map: ElementInfo -> Node
    map = createTag Map
    public export
    area: ElementInfo -> Node
    area = createTag Area
    public export
    math: ElementInfo -> Node
    math = createTag Math
    public export
    sVG: ElementInfo -> Node
    sVG = createTag SVG
    public export
    table: ElementInfo -> Node
    table = createTag Table
    public export
    caption: ElementInfo -> Node
    caption = createTag Caption
    public export
    colgroup: ElementInfo -> Node
    colgroup = createTag Colgroup
    public export
    col: ElementInfo -> Node
    col = createTag Col
    public export
    tbody: ElementInfo -> Node
    tbody = createTag Tbody
    public export
    thead: ElementInfo -> Node
    thead = createTag Thead
    public export
    tfoot: ElementInfo -> Node
    tfoot = createTag Tfoot
    public export
    tr: ElementInfo -> Node
    tr = createTag Tr
    public export
    td: ElementInfo -> Node
    td = createTag Td
    public export
    th: ElementInfo -> Node
    th = createTag Th
    public export
    script: ElementInfo -> Node
    script = createTag Script
    public export
    template: ElementInfo -> Node
    template = createTag Template
    public export
    form: ElementInfo -> Node
    form = createTag Form
    public export
    label: ElementInfo -> Node
    label = createTag Label
    public export
    input: ElementInfo -> Node
    input = createTag Input
    public export
    button: ElementInfo -> Node
    button = createTag Button
    public export
    select: ElementInfo -> Node
    select = createTag Select
    public export
    datalist: ElementInfo -> Node
    datalist = createTag Datalist
    public export
    optgroup: ElementInfo -> Node
    optgroup = createTag Optgroup
    public export
    option: ElementInfo -> Node
    option = createTag Option
    public export
    textarea: ElementInfo -> Node
    textarea = createTag Textarea
    public export
    output: ElementInfo -> Node
    output = createTag Output
    public export
    progress: ElementInfo -> Node
    progress = createTag Progress
    public export
    meter: ElementInfo -> Node
    meter = createTag Meter
    public export
    fieldset: ElementInfo -> Node
    fieldset = createTag Fieldset
    public export
    legend: ElementInfo -> Node
    legend = createTag Legend
    public export
    details: ElementInfo -> Node
    details = createTag Details
    public export
    summary: ElementInfo -> Node
    summary = createTag Summary
    public export
    dialog: ElementInfo -> Node
    dialog = createTag Dialog
    public export
    selectedContent: ElementInfo -> Node
    selectedContent = createTag SelectedContent
    public export
    custom: String -> ElementInfo -> Node
    custom name = createTag (Custom name)

||| Defines a function for defining every attribute
namespace Attribute
    public export
    accesskey: String -> (Attribute,String)
    accesskey = (Accesskey,)
    public export
    abbr: String -> (Attribute,String)
    abbr = (Abbr,)
    public export
    accept: String -> (Attribute,String)
    accept = (Accept,)
    public export
    acceptCharset: String -> (Attribute,String)
    acceptCharset = (AcceptCharset,)
    public export
    action: String -> (Attribute,String)
    action = (Action,)
    public export
    allow: String -> (Attribute,String)
    allow = (Allow,)
    public export
    allowfullscreen: String -> (Attribute,String)
    allowfullscreen = (Allowfullscreen,)
    public export
    alpha: String -> (Attribute,String)
    alpha = (Alpha,)
    public export
    alt: String -> (Attribute,String)
    alt = (Alt,)
    public export
    as: String -> (Attribute,String)
    as = (As,)
    public export
    async: String -> (Attribute,String)
    async = (Async,)
    public export
    autocapitalize: String -> (Attribute,String)
    autocapitalize = (Autocapitalize,)
    public export
    autocomplete: String -> (Attribute,String)
    autocomplete = (Autocomplete,)
    public export
    autocorrect: String -> (Attribute,String)
    autocorrect = (Autocorrect,)
    public export
    autofocus: String -> (Attribute,String)
    autofocus = (Autofocus,)
    public export
    autoplay: String -> (Attribute,String)
    autoplay = (Autoplay,)
    public export
    blocking: String -> (Attribute,String)
    blocking = (Blocking,)
    public export
    charset: String -> (Attribute,String)
    charset = (Charset,)
    public export
    checked: String -> (Attribute,String)
    checked = (Checked,)
    public export
    cite: String -> (Attribute,String)
    cite = (Cite,)
    public export
    class: String -> (Attribute,String)
    class = (Class,)
    public export
    closedBy: String -> (Attribute,String)
    closedBy = (ClosedBy,)
    public export
    color: String -> (Attribute,String)
    color = (Color,)
    public export
    colorspace: String -> (Attribute,String)
    colorspace = (Colorspace,)
    public export
    cols: String -> (Attribute,String)
    cols = (Cols,)
    public export
    colspan: String -> (Attribute,String)
    colspan = (Colspan,)
    public export
    command: String -> (Attribute,String)
    command = (Command,)
    public export
    commandfor: String -> (Attribute,String)
    commandfor = (Commandfor,)
    public export
    content: String -> (Attribute,String)
    content = (Content,)
    public export
    contenteditable: String -> (Attribute,String)
    contenteditable = (Contenteditable,)
    public export
    controls: String -> (Attribute,String)
    controls = (Controls,)
    public export
    coords: String -> (Attribute,String)
    coords = (Coords,)
    public export
    crossorigin: String -> (Attribute,String)
    crossorigin = (Crossorigin,)
    public export
    data': String -> (Attribute,String)
    data' = (Data,)
    public export
    datetime: String -> (Attribute,String)
    datetime = (Datetime,)
    public export
    decoding: String -> (Attribute,String)
    decoding = (Decoding,)
    public export
    default': String -> (Attribute,String)
    default' = (Default,)
    public export
    defer: String -> (Attribute,String)
    defer = (Defer,)
    public export
    dir: String -> (Attribute,String)
    dir = (Dir,)
    public export
    dirname: String -> (Attribute,String)
    dirname = (Dirname,)
    public export
    disabled: String -> (Attribute,String)
    disabled = (Disabled,)
    public export
    download: String -> (Attribute,String)
    download = (Download,)
    public export
    draggable: String -> (Attribute,String)
    draggable = (Draggable,)
    public export
    enctype: String -> (Attribute,String)
    enctype = (Enctype,)
    public export
    enterkeyhint: String -> (Attribute,String)
    enterkeyhint = (Enterkeyhint,)
    public export
    fetchpriority: String -> (Attribute,String)
    fetchpriority = (Fetchpriority,)
    public export
    for: String -> (Attribute,String)
    for = (For,)
    public export
    form: String -> (Attribute,String)
    form = (Form,)
    public export
    formaction: String -> (Attribute,String)
    formaction = (Formaction,)
    public export
    formenctype: String -> (Attribute,String)
    formenctype = (Formenctype,)
    public export
    formmethod: String -> (Attribute,String)
    formmethod = (Formmethod,)
    public export
    formnovalidate: String -> (Attribute,String)
    formnovalidate = (Formnovalidate,)
    public export
    formtarget: String -> (Attribute,String)
    formtarget = (Formtarget,)
    public export
    headers: String -> (Attribute,String)
    headers = (Headers,)
    public export
    headingoffset: String -> (Attribute,String)
    headingoffset = (Headingoffset,)
    public export
    headingreset: String -> (Attribute,String)
    headingreset = (Headingreset,)
    public export
    height: String -> (Attribute,String)
    height = (Height,)
    public export
    hidden: String -> (Attribute,String)
    hidden = (Hidden,)
    public export
    high: String -> (Attribute,String)
    high = (High,)
    public export
    href: String -> (Attribute,String)
    href = (Href,)
    public export
    hreflang: String -> (Attribute,String)
    hreflang = (Hreflang,)
    public export
    httpEquiv: String -> (Attribute,String)
    httpEquiv = (HttpEquiv,)
    public export
    id: String -> (Attribute,String)
    id = (Id,)
    public export
    imagesizes: String -> (Attribute,String)
    imagesizes = (Imagesizes,)
    public export
    imagesrcset: String -> (Attribute,String)
    imagesrcset = (Imagesrcset,)
    public export
    inert: String -> (Attribute,String)
    inert = (Inert,)
    public export
    inputmode: String -> (Attribute,String)
    inputmode = (Inputmode,)
    public export
    integrity: String -> (Attribute,String)
    integrity = (Integrity,)
    public export
    is: String -> (Attribute,String)
    is = (Is,)
    public export
    ismap: String -> (Attribute,String)
    ismap = (Ismap,)
    public export
    itemid: String -> (Attribute,String)
    itemid = (Itemid,)
    public export
    itemprop: String -> (Attribute,String)
    itemprop = (Itemprop,)
    public export
    itemref: String -> (Attribute,String)
    itemref = (Itemref,)
    public export
    itemscope: String -> (Attribute,String)
    itemscope = (Itemscope,)
    public export
    itemtype: String -> (Attribute,String)
    itemtype = (Itemtype,)
    public export
    kind: String -> (Attribute,String)
    kind = (Kind,)
    public export
    label': String -> (Attribute,String)
    label' = (Label,)
    public export
    lang: String -> (Attribute,String)
    lang = (Lang,)
    public export
    list: String -> (Attribute,String)
    list = (List,)
    public export
    loading: String -> (Attribute,String)
    loading = (Loading,)
    public export
    loop: String -> (Attribute,String)
    loop = (Loop,)
    public export
    low: String -> (Attribute,String)
    low = (Low,)
    public export
    max: String -> (Attribute,String)
    max = (Max,)
    public export
    maxlength: String -> (Attribute,String)
    maxlength = (Maxlength,)
    public export
    media: String -> (Attribute,String)
    media = (Media,)
    public export
    method: String -> (Attribute,String)
    method = (Method,)
    public export
    min: String -> (Attribute,String)
    min = (Min,)
    public export
    minlength: String -> (Attribute,String)
    minlength = (Minlength,)
    public export
    multiple: String -> (Attribute,String)
    multiple = (Multiple,)
    public export
    muted: String -> (Attribute,String)
    muted = (Muted,)
    public export
    name: String -> (Attribute,String)
    name = (Name,)
    public export
    nonce: String -> (Attribute,String)
    nonce = (Nonce,)
    public export
    nomodule: String -> (Attribute,String)
    nomodule = (Nomodule,)
    public export
    novalidate: String -> (Attribute,String)
    novalidate = (Novalidate,)
    public export
    open': String -> (Attribute,String)
    open' = (Open,)
    public export
    optimum: String -> (Attribute,String)
    optimum = (Optimum,)
    public export
    pattern: String -> (Attribute,String)
    pattern = (Pattern,)
    public export
    ping: String -> (Attribute,String)
    ping = (Ping,)
    public export
    placeholder: String -> (Attribute,String)
    placeholder = (Placeholder,)
    public export
    playsinline: String -> (Attribute,String)
    playsinline = (Playsinline,)
    public export
    popover: String -> (Attribute,String)
    popover = (Popover,)
    public export
    popovertarget: String -> (Attribute,String)
    popovertarget = (Popovertarget,)
    public export
    popovertargetaction: String -> (Attribute,String)
    popovertargetaction = (Popovertargetaction,)
    public export
    poster: String -> (Attribute,String)
    poster = (Poster,)
    public export
    preload: String -> (Attribute,String)
    preload = (Preload,)
    public export
    readonly: String -> (Attribute,String)
    readonly = (Readonly,)
    public export
    referrerpolicy: String -> (Attribute,String)
    referrerpolicy = (Referrerpolicy,)
    public export
    rel: String -> (Attribute,String)
    rel = (Rel,)
    public export
    required: String -> (Attribute,String)
    required = (Required,)
    public export
    reversed: String -> (Attribute,String)
    reversed = (Reversed,)
    public export
    rows: String -> (Attribute,String)
    rows = (Rows,)
    public export
    rowspan: String -> (Attribute,String)
    rowspan = (Rowspan,)
    public export
    sandbox: String -> (Attribute,String)
    sandbox = (Sandbox,)
    public export
    scope: String -> (Attribute,String)
    scope = (Scope,)
    public export
    selected: String -> (Attribute,String)
    selected = (Selected,)
    public export
    shadowrootmode: String -> (Attribute,String)
    shadowrootmode = (Shadowrootmode,)
    public export
    shadowrootdelegatesfocus: String -> (Attribute,String)
    shadowrootdelegatesfocus = (Shadowrootdelegatesfocus,)
    public export
    shadowrootslotassignment: String -> (Attribute,String)
    shadowrootslotassignment = (Shadowrootslotassignment,)
    public export
    shadowrootclonable: String -> (Attribute,String)
    shadowrootclonable = (Shadowrootclonable,)
    public export
    shadowrootserializable: String -> (Attribute,String)
    shadowrootserializable = (Shadowrootserializable,)
    public export
    shadowrootcustomelementregistry: String -> (Attribute,String)
    shadowrootcustomelementregistry = (Shadowrootcustomelementregistry,)
    public export
    shape: String -> (Attribute,String)
    shape = (Shape,)
    public export
    size: String -> (Attribute,String)
    size = (Size,)
    public export
    sizes: String -> (Attribute,String)
    sizes = (Sizes,)
    public export
    slot: String -> (Attribute,String)
    slot = (Slot,)
    public export
    span': String -> (Attribute,String)
    span' = (Span,)
    public export
    spellcheck: String -> (Attribute,String)
    spellcheck = (Spellcheck,)
    public export
    src: String -> (Attribute,String)
    src = (Src,)
    public export
    srcdoc: String -> (Attribute,String)
    srcdoc = (Srcdoc,)
    public export
    srclang: String -> (Attribute,String)
    srclang = (Srclang,)
    public export
    srcset: String -> (Attribute,String)
    srcset = (Srcset,)
    public export
    start: String -> (Attribute,String)
    start = (Start,)
    public export
    step: String -> (Attribute,String)
    step = (Step,)
    public export
    style: String -> (Attribute,String)
    style = (Style,)
    public export
    tabindex: String -> (Attribute,String)
    tabindex = (Tabindex,)
    public export
    target: String -> (Attribute,String)
    target = (Target,)
    public export
    title': String -> (Attribute,String)
    title' = (Title,)
    public export
    translate: String -> (Attribute,String)
    translate = (Translate,)
    public export
    type: String -> (Attribute,String)
    type = (Type',)
    public export
    usemap: String -> (Attribute,String)
    usemap = (Usemap,)
    public export
    value: String -> (Attribute,String)
    value = (Value,)
    public export
    width: String -> (Attribute,String)
    width = (Width,)
    public export
    wrap: String -> (Attribute,String)
    wrap = (Wrap,)
    public export
    writingsuggestions: String -> (Attribute,String)
    writingsuggestions = (Writingsuggestions,)
    public export
    onauxclick: String -> (Attribute,String)
    onauxclick = (Onauxclick,)
    public export
    onbeforeinput: String -> (Attribute,String)
    onbeforeinput = (Onbeforeinput,)
    public export
    onbeforematch: String -> (Attribute,String)
    onbeforematch = (Onbeforematch,)
    public export
    onbeforetoggle: String -> (Attribute,String)
    onbeforetoggle = (Onbeforetoggle,)
    public export
    onblur: String -> (Attribute,String)
    onblur = (Onblur,)
    public export
    oncancel: String -> (Attribute,String)
    oncancel = (Oncancel,)
    public export
    oncanplay: String -> (Attribute,String)
    oncanplay = (Oncanplay,)
    public export
    oncanplaythrough: String -> (Attribute,String)
    oncanplaythrough = (Oncanplaythrough,)
    public export
    onchange: String -> (Attribute,String)
    onchange = (Onchange,)
    public export
    onclick: String -> (Attribute,String)
    onclick = (Onclick,)
    public export
    onclose: String -> (Attribute,String)
    onclose = (Onclose,)
    public export
    oncommand: String -> (Attribute,String)
    oncommand = (Oncommand,)
    public export
    oncontextlost: String -> (Attribute,String)
    oncontextlost = (Oncontextlost,)
    public export
    oncontextmenu: String -> (Attribute,String)
    oncontextmenu = (Oncontextmenu,)
    public export
    oncontextrestored: String -> (Attribute,String)
    oncontextrestored = (Oncontextrestored,)
    public export
    oncopy: String -> (Attribute,String)
    oncopy = (Oncopy,)
    public export
    oncuechange: String -> (Attribute,String)
    oncuechange = (Oncuechange,)
    public export
    oncut: String -> (Attribute,String)
    oncut = (Oncut,)
    public export
    ondblclick: String -> (Attribute,String)
    ondblclick = (Ondblclick,)
    public export
    ondrag: String -> (Attribute,String)
    ondrag = (Ondrag,)
    public export
    ondragend: String -> (Attribute,String)
    ondragend = (Ondragend,)
    public export
    ondragenter: String -> (Attribute,String)
    ondragenter = (Ondragenter,)
    public export
    ondragleave: String -> (Attribute,String)
    ondragleave = (Ondragleave,)
    public export
    ondragover: String -> (Attribute,String)
    ondragover = (Ondragover,)
    public export
    ondragstart: String -> (Attribute,String)
    ondragstart = (Ondragstart,)
    public export
    ondrop: String -> (Attribute,String)
    ondrop = (Ondrop,)
    public export
    ondurationchange: String -> (Attribute,String)
    ondurationchange = (Ondurationchange,)
    public export
    onemptied: String -> (Attribute,String)
    onemptied = (Onemptied,)
    public export
    onended: String -> (Attribute,String)
    onended = (Onended,)
    public export
    onerror: String -> (Attribute,String)
    onerror = (Onerror,)
    public export
    onfocus: String -> (Attribute,String)
    onfocus = (Onfocus,)
    public export
    onformdata: String -> (Attribute,String)
    onformdata = (Onformdata,)
    public export
    oninput: String -> (Attribute,String)
    oninput = (Oninput,)
    public export
    oninvalid: String -> (Attribute,String)
    oninvalid = (Oninvalid,)
    public export
    onkeydown: String -> (Attribute,String)
    onkeydown = (Onkeydown,)
    public export
    onkeypress: String -> (Attribute,String)
    onkeypress = (Onkeypress,)
    public export
    onkeyup: String -> (Attribute,String)
    onkeyup = (Onkeyup,)
    public export
    onload: String -> (Attribute,String)
    onload = (Onload,)
    public export
    onloadeddata: String -> (Attribute,String)
    onloadeddata = (Onloadeddata,)
    public export
    onloadedmetadata: String -> (Attribute,String)
    onloadedmetadata = (Onloadedmetadata,)
    public export
    onloadstart: String -> (Attribute,String)
    onloadstart = (Onloadstart,)
    public export
    onmousedown: String -> (Attribute,String)
    onmousedown = (Onmousedown,)
    public export
    onmouseenter: String -> (Attribute,String)
    onmouseenter = (Onmouseenter,)
    public export
    onmouseleave: String -> (Attribute,String)
    onmouseleave = (Onmouseleave,)
    public export
    onmousemove: String -> (Attribute,String)
    onmousemove = (Onmousemove,)
    public export
    onmouseout: String -> (Attribute,String)
    onmouseout = (Onmouseout,)
    public export
    onmouseover: String -> (Attribute,String)
    onmouseover = (Onmouseover,)
    public export
    onmouseup: String -> (Attribute,String)
    onmouseup = (Onmouseup,)
    public export
    onpaste: String -> (Attribute,String)
    onpaste = (Onpaste,)
    public export
    onpause: String -> (Attribute,String)
    onpause = (Onpause,)
    public export
    onplay: String -> (Attribute,String)
    onplay = (Onplay,)
    public export
    onplaying: String -> (Attribute,String)
    onplaying = (Onplaying,)
    public export
    onprogress: String -> (Attribute,String)
    onprogress = (Onprogress,)
    public export
    onratechange: String -> (Attribute,String)
    onratechange = (Onratechange,)
    public export
    onreset: String -> (Attribute,String)
    onreset = (Onreset,)
    public export
    onresize: String -> (Attribute,String)
    onresize = (Onresize,)
    public export
    onscroll: String -> (Attribute,String)
    onscroll = (Onscroll,)
    public export
    onscrollend: String -> (Attribute,String)
    onscrollend = (Onscrollend,)
    public export
    onsecuritypolicyviolation: String -> (Attribute,String)
    onsecuritypolicyviolation = (Onsecuritypolicyviolation,)
    public export
    onseeked: String -> (Attribute,String)
    onseeked = (Onseeked,)
    public export
    onseeking: String -> (Attribute,String)
    onseeking = (Onseeking,)
    public export
    onselect: String -> (Attribute,String)
    onselect = (Onselect,)
    public export
    onslotchange: String -> (Attribute,String)
    onslotchange = (Onslotchange,)
    public export
    onstalled: String -> (Attribute,String)
    onstalled = (Onstalled,)
    public export
    onsubmit: String -> (Attribute,String)
    onsubmit = (Onsubmit,)
    public export
    onsuspend: String -> (Attribute,String)
    onsuspend = (Onsuspend,)
    public export
    ontimeupdate: String -> (Attribute,String)
    ontimeupdate = (Ontimeupdate,)
    public export
    ontoggle: String -> (Attribute,String)
    ontoggle = (Ontoggle,)
    public export
    onvolumechange: String -> (Attribute,String)
    onvolumechange = (Onvolumechange,)
    public export
    onwaiting: String -> (Attribute,String)
    onwaiting = (Onwaiting,)
    public export
    onwheel: String -> (Attribute,String)
    onwheel = (Onwheel,)
    public export
    onafterprint: String -> (Attribute,String)
    onafterprint = (Onafterprint,)
    public export
    onbeforeprint: String -> (Attribute,String)
    onbeforeprint = (Onbeforeprint,)
    public export
    onbeforeunload: String -> (Attribute,String)
    onbeforeunload = (Onbeforeunload,)
    public export
    onhashchange: String -> (Attribute,String)
    onhashchange = (Onhashchange,)
    public export
    onlanguagechange: String -> (Attribute,String)
    onlanguagechange = (Onlanguagechange,)
    public export
    onmessage: String -> (Attribute,String)
    onmessage = (Onmessage,)
    public export
    onmessageerror: String -> (Attribute,String)
    onmessageerror = (Onmessageerror,)
    public export
    onoffline: String -> (Attribute,String)
    onoffline = (Onoffline,)
    public export
    ononline: String -> (Attribute,String)
    ononline = (Ononline,)
    public export
    onpagehide: String -> (Attribute,String)
    onpagehide = (Onpagehide,)
    public export
    onpagereveal: String -> (Attribute,String)
    onpagereveal = (Onpagereveal,)
    public export
    onpageshow: String -> (Attribute,String)
    onpageshow = (Onpageshow,)
    public export
    onpageswap: String -> (Attribute,String)
    onpageswap = (Onpageswap,)
    public export
    onpopstate: String -> (Attribute,String)
    onpopstate = (Onpopstate,)
    public export
    onrejectionhandled: String -> (Attribute,String)
    onrejectionhandled = (Onrejectionhandled,)
    public export
    onstorage: String -> (Attribute,String)
    onstorage = (Onstorage,)
    public export
    onunhandledrejection: String -> (Attribute,String)
    onunhandledrejection = (Onunhandledrejection,)
    public export
    onunload: String -> (Attribute,String)
    onunload = (Onunload,)
    public export
    custom: String -> String -> (Attribute,String)
    custom string = (Custom string,)