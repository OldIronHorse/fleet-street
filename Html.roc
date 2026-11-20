import Url

Html := {}.{
    Doc := {
        lang: Str ?? "en",
        head: Head,
        body: List(Block) ?? []
    }.{
        with_head : Head -> Doc
        with_head = |head|
            Doc.{ head }

        with_lang : Doc, Str -> Doc
        with_lang = |doc, lang| { ..doc, lang }

        with_body : Doc, List(Block) -> Doc
        with_body = |doc, body| { ..doc, body }

        render : Doc -> Str
        render = |doc|
            "<!DOCTYPE html>\n<html lang=\"${escape(doc.lang)}\">${doc.head.render()}${render_body(doc.body)}</html>"
    }

    Head := { title: Str, css: List(Url) ?? []  }.{
        render : Head -> Str
        render = |h|
            "<head><meta charset=\"utf-8\"><title>${escape(h.title)}</title>${render_css_links(h.css)}</head>"
    }

    Inline := [
        Text(Str),
        Strong(List(Inline)),
        Emphasis(List(Inline)),
        Code({ attrs: Attrs, content: Str }),
        Span(InlinesSpec),
        Link({ label: List(LinkContent), url: Url }),
        Image(ImageSpec),
    ].{
        is_eq = |this, that|
            match (this, that) {
                (Text(a), Text(b)) => a == b
                (Strong(a), Strong(b)) => a == b
                (Emphasis(a), Emphasis(b)) => a == b
                (Code(a), Code(b)) => a == b
                (Span(a), Span(b)) => a == b
                (Link(a), Link(b)) => a == b
                (Image(a), Image(b)) => a == b
                _ => False
            }
    }

    LinkContent := [
        Text(Str),
        Strong(List(LinkContent)),
        Emphasis(List(LinkContent)),
        Image(ImageSpec),
        Span({ attrs: Attrs, contents: List(LinkContent) })
        Code({ attrs: Attrs, content: Str }),
    ].{
        to_inline : LinkContent -> Inline
        to_inline = |c|
            match c {
                Text(s) => Text(s)
                Strong(cs) => Strong(cs.map(to_inline))
                Emphasis(cs) => Emphasis(cs.map(to_inline))
                Span(ss) => Span({ attrs: ss.attrs, contents: ss.contents.map(to_inline) })
                Code(cs) => Code({ attrs: cs.attrs, content: cs.content })
                Image(s) => Image(s)
            }

        is_eq = |this, that|
            match (this, that) {
                (Text(a), Text(b)) => a == b
                (Strong(a), Strong(b)) => a == b
                (Emphasis(a), Emphasis(b)) => a == b
                (Image(a), Image(b)) => a == b
                (Code(a), Code(b)) => a == b
                (Span(a), Span(b)) => a == b
                _ => False
            }
    }

    Block := [
        Heading(HeadingSpec),
        Paragraph(InlinesSpec),
        UnorderedList(ContentSpec),
        OrderedList(ContentSpec),
        Div(BlocksSpec),
        Table(TableSpec),
        BlockQuote(BlocksSpec),
        Pre(InlinesSpec),
        Rule(Attrs),
    ].{
        is_eq = |this, that|
            match (this, that) {
                (Heading(a), Heading(b)) => a == b
                (Paragraph(a), Paragraph(b)) => a == b
                (UnorderedList(a), UnorderedList(b)) => a == b
                (OrderedList(a), OrderedList(b)) => a == b
                (Div(a), Div(b)) => a == b
                (Pre(a), Pre(b)) => a == b
                (Table(a), Table(b)) => a == b
                (BlockQuote(a), BlockQuote(b)) => a == b
                (Rule(a), Rule(b)) => a == b
                _ => False
            }
    }

    Alignment : [Default, Left, Center, Right]

    TableConfig := {
        attrs: Attrs ?? Attrs.{},
        caption: Str ?? "",
        headers: List(Content) ?? [],
        align: List(Alignment)?? [],
        data: List(List(Content)),
    }

    # TableSpec is opaque to enforce the rectangular table check
    TableSpec :: {
        attrs: Attrs,
        caption: Str,
        headers: List(Content),
        align: List(Alignment),
        data: List(List(Content)),
    }.{
        new : TableConfig -> Try(TableSpec, [RaggedRows])
        new = |tc| {
            rows = if tc.headers.is_empty() tc.data else tc.data.prepend(tc.headers)
            rectangular = match rows {
                [first, .. as rest] => rest.all(|row| row.len() == first.len())
                [] => True
            }
            if rectangular {
                width = rows.first().ok_or([]).len()
                Ok({
                    attrs: tc.attrs,
                    caption: tc.caption,
                    align: tc.align.concat(List.repeat(Default, width)).take_first(width),
                    headers: tc.headers,
                    data: tc.data,
                })
            }
            else
                Err(RaggedRows)
        }

        is_eq = |this, that|
            this.attrs == that.attrs
                and this.caption == that.caption
                and this.headers == that.headers
                and this.align == that.align
                and this.data == that.data
    }

    BlocksSpec := {
        attrs: Attrs ?? Attrs.{},
        contents: List(Block),
    }.{
        is_eq = |this, that|
            this.attrs == that.attrs
                and this.contents == that.contents
    }

    ImageSpec := {
        attrs:  Attrs ?? Attrs.{},
        url: Url,
        alt: Str,
        height: U64 ?? 0,
        width: U64 ?? 0,
    }.{
        is_eq = |this, that|
            this.attrs == that.attrs
                and this.url == that.url
                and this.alt == that.alt
                and this.height == that.height
                and this.width == that.width
    }

    ContentSpec := {
        attrs: Attrs ?? Attrs.{},
        items: List(Content),
    }.{
        is_eq = |this, that|
            this.attrs == that.attrs
                and this.items == that.items
    }

    Content := [
        Inlines(List(Inline)),
        Blocks(List(Block)),
    ].{
        is_eq = |this, that|
            match (this, that) {
                (Inlines(a), Inlines(b)) => a == b
                (Blocks(a), Blocks(b)) => a == b
                _ => False
            }
    }

    Attrs := {
        id: Str ?? "",
        classes: List(Str) ?? [],
    }.{
        is_eq = |this, that|
            this.id == that.id
                and this.classes == that.classes
    }

    HeadingSpec := {
        attrs: Attrs ?? Attrs.{},
        level: Level,
        contents: List(Inline),
    }.{
        is_eq = |this, that|
            this.attrs == that.attrs
                and this.level == that.level
                and this.contents == that.contents
    }

    InlinesSpec := {
        attrs: Attrs ?? Attrs.{},
        contents: List(Inline)
    }.{
        is_eq = |this, that|
            this.attrs == that.attrs
                and this.contents == that.contents
    }

    Level := [H1, H2, H3, H4, H5, H6].{
        to_str : Level -> Str
        to_str  = |l|
            match l {
                H1 => "1"
                H2 => "2"
                H3 => "3"
                H4 => "4"
                H5 => "5"
                H6 => "6"
            }

        is_eq = |this, that|
            match (this, that) {
                (H1, H1) | (H2, H2) | (H3, H3) | (H4, H4) | (H5, H5) |(H6, H6) => True
                _ => False
            }
    }
}

render_css_links : List(Url) -> Str
render_css_links = |urls|
    urls.map(|url| "<link rel=\"stylesheet\" href=\"${escape(url.to_str())}\">")
        |> Str.join_with("")

render_body : List(Block) -> Str
render_body = |body|
    "<body>${body.map(render_block) |> Str.join_with("")}</body>"

render_inline : Inline -> Str
render_inline = |c|
    match c {
        Text(s) => escape(s)
        Strong(content) => "<strong>${render_inlines(content)}</strong>"
        Emphasis(content) => "<em>${render_inlines(content)}</em>"
        Link({ url, label }) =>
            "<a href=\"${escape(url.to_str())}\">${label.map(|l| l.to_inline()) |> render_inlines()}</a>"
        Image(s) => {
            width = if s.width == 0 "" else " width=\"${s.width.to_str()}\""
            height = if s.height == 0 "" else " height=\"${s.height.to_str()}\""
            "<img${render_attrs(s.attrs)} src=\"${escape(s.url.to_str())}\" alt=\"${escape(s.alt)}\"${width}${height}>"
        }
        Code(cs) =>
            "<code${render_attrs(cs.attrs)}>${escape(cs.content)}</code>"
        Span(cs) =>
            "<span${render_attrs(cs.attrs)}>${render_inlines(cs.contents)}</span>"
    }

render_inlines : List(Inline) -> Str
render_inlines = |content|
    content.map(render_inline)
        |> Str.join_with("")

render_list_contents : List(Content) -> Str
render_list_contents = |items|
    items.map(render_list_content)
        |> Str.join_with("")

render_list_content : Content -> Str
render_list_content = |item| "<li>${render_content(item)}</li>"

render_content : Content -> Str
render_content = |items|
    match items {
        Inlines(ils) => render_inlines(ils)
        Blocks(bs) => bs.map(render_block) |> Str.join_with("")
    }

escape : Str -> Str
escape = |s|
    s.replace_each("&", "&amp;")
        .replace_each(">", "&gt;")
        .replace_each("<", "&lt;")
        .replace_each("\"", "&quot;")

render_block : Block -> Str
render_block = |b|
    match b {
        Heading(hs) =>
            "<h${hs.level.to_str()}${render_attrs(hs.attrs)}>${render_inlines(hs.contents)}</h${hs.level.to_str()}>"
        Paragraph(ps) =>
            "<p${render_attrs(ps.attrs)}>${render_inlines(ps.contents)}</p>"
        UnorderedList(ls) =>
            "<ul${render_attrs(ls.attrs)}>${render_list_contents(ls.items)}</ul>"
        OrderedList(ls) =>
            "<ol${render_attrs(ls.attrs)}>${render_list_contents(ls.items)}</ol>"
        Div(ds) =>
            "<div${render_attrs(ds.attrs)}>${ds.contents.map(render_block) |> Str.join_with("")}</div>"
        Table(ts) => {
            caption = if ts.caption.is_empty() "" else "<caption>${escape(ts.caption)}</caption>"
            align = ts.align.map(render_align)
            hs = ts.headers.map2(align, |h, a| "<th scope=\"col\"${a}>${render_content(h)}</th>")
            rows = ts.data.map(|r| render_table_row(r, align))
            body = if rows.is_empty() "" else "<tbody>${rows |> Str.join_with("")}</tbody>"
            "<table${render_attrs(ts.attrs)}>${caption}${if hs.is_empty() "" else "<thead><tr>".concat(hs |> Str.join_with("")).concat("</tr></thead>")}${body}</table>"
        }
        BlockQuote(bqs) =>
            "<blockquote${render_attrs(bqs.attrs)}>${bqs.contents.map(render_block) |> Str.join_with("")}</blockquote>"
        Pre(ps) =>
            "<pre${render_attrs(ps.attrs)}>${render_inlines(ps.contents)}</pre>"
        Rule(attrs) => "<hr${render_attrs(attrs)}>"
    }

render_align : Alignment -> Str
render_align = |a|
    match a {
        Default => ""
        Left => " class=\"align-left\""
        Right => " class=\"align-right\""
        Center => " class=\"align-center\""
    }

render_table_row : List(Content), List(Str) -> Str
render_table_row = |cells, align|
    "<tr>${cells.map2(align, |c, a| "<td".concat(a).concat(">").concat(render_content(c)).concat("</td>")) |> Str.join_with("")}</tr>"

render_attrs : Attrs -> Str
render_attrs = |attrs| {
    id_ = if attrs.id.is_empty() "" else " id=\"${escape(attrs.id)}\""
    class = match attrs.classes.drop_if(Str.is_empty) {
        [] => ""
        cs => " class=\"${cs.map(escape) |> Str.join_with(" ")}\""
    }
    id_.concat(class)
}

expect "a &gt; b &amp; c &lt; d &quot;" == Text("a > b & c < d \"") |> render_inline()

expect "<h1>Shopping List</h1>"
    == Heading(HeadingSpec.{ level: H1, contents: [Text("Shopping List")] })
        |> render_block()
expect "<h1 id=\"id-list-heading\">Shopping List</h1>"
    == Heading(HeadingSpec.{
        attrs: { id: "id-list-heading" },
        level: H1,
        contents: [Text("Shopping List")]
    }) |> render_block()
expect "<p></p>" == Paragraph(InlinesSpec.{ contents: [] }) |> render_block()
expect "<p>Shopping List</p>"
    == Paragraph(InlinesSpec.{ contents: [Text("Shopping List")] })
        |> render_block()
expect "<p class=\"todo\">Shopping List</p>"
    == Paragraph(InlinesSpec.{
        attrs: { classes: ["todo"] },
        contents: [Text("Shopping List")]
    }) |> render_block()
expect "<p>Shopping List</p>"
    == Paragraph(InlinesSpec.{
        attrs: { classes: [""] },
        contents: [Text("Shopping List")]
    }) |> render_block()
expect "<p class=\"todo b&quot;ox\">Shopping List</p>"
    == Paragraph(InlinesSpec.{
        attrs: { classes: ["todo", "b\"ox"] },
        contents: [Text("Shopping List")]
    }) |> render_block()
expect {
    expected = "<p id=\"id-list-&quot;heading&quot;\" class=\"todo\">Shopping List</p>"
    actual = Paragraph(
        InlinesSpec.{
            attrs: {
                id: "id-list-\"heading\"",
                classes: ["todo"],
            },
            contents: [Text("Shopping List")]
        }) |> render_block()
    actual == expected
}
expect "<p>Shopping <strong>List</strong></p>"
    == Paragraph(InlinesSpec.{ contents: [Text("Shopping "), Strong([Text("List")])] })
        |> render_block()
expect "<p>Shopping <strong><strong>List</strong></strong></p>"
    == Paragraph(InlinesSpec.{ contents: [Text("Shopping "), Strong([Strong([Text("List")])])] })
        |> render_block()

expect \\<!DOCTYPE html>
       \\<html lang="en"><head><meta charset="utf-8"><title>12&lt;34</title></head><body></body></html>
    == Doc.with_head({ title: "12<34" })
        .render()
expect \\<!DOCTYPE html>
       \\<html lang="en&quot;x"><head><meta charset="utf-8"><title>12&lt;34</title></head><body></body></html>
    == Doc.with_head({ title: "12<34" })
        .with_lang("en\"x")
        .render()
expect {
    expected = \\<!DOCTYPE html>
               \\<html lang="en"><head><meta charset="utf-8"><title>abcde</title></head><body><h1>Shopping List</h1></body></html>
    actual = Doc.with_head({ title: "abcde" })
        .with_body([Heading({ level: H1, contents: [Text("Shopping List")] })])
        .render()
    actual == expected
}
expect {
    expected = \\<!DOCTYPE html>
               \\<html lang="en"><head><meta charset="utf-8"><title>abcde</title><link rel="stylesheet" href="../styles/simple.css"></head><body></body></html>
    actual = Doc.with_head({ title: "abcde", css: [Relative({ path: ["..", "styles", "simple.css"] })] })
        .render()
    actual == expected
}
expect \\<!DOCTYPE html>
       \\<html lang="en"><head><meta charset="utf-8"><title>1234</title></head><body><ul><li>first</li></ul></body></html>
    == Doc.with_head({ title: "1234" })
        .with_body([UnorderedList({ items: [Inlines([Text("first")])] })])
        .render()

expect "<body><p>first</p><p>second</p><p>third</p></body>"
    == [
        Paragraph({ contents: [Text("first")] }),
        Paragraph({ contents: [Text("second")] }),
        Paragraph({ contents: [Text("third")] }),
    ] |> render_body()

expect "<a href=\"https://example.com/one%20mor%C3%A9%3F%23%2F%25/two.html\">The Link</a>"
    == Link({
        url: Http({ host: Url.Host.from_str("example.com")?, path: ["one moré?#/%", "two.html"] }),
        label: [Text("The Link")],
    }) |> render_inline()
expect {
    expected ="<a href=\"mailto:someone%40example.com?subject=Feedback%20Request%20%26%20Link&amp;body=Please%20provide%20feedback.&amp;cc=cc1%40elsewhere.com,cc2%40example.com&amp;bcc=bc1%40elsewhere.com,bc2%40example.com\">Send Feedback</a>"
    actual = Link({
        url: MailTo({
            to: [{ recipient: "someone", domain: Url.Host.from_str("example.com")? }],
            subject: "Feedback Request & Link",
            body: "Please provide feedback.",
            cc: [
                { recipient: "cc1", domain: Url.Host.from_str("elsewhere.com")? },
                { recipient: "cc2", domain: Url.Host.from_str("example.com")? },
            ],
            bcc: [
                { recipient: "bc1", domain: Url.Host.from_str("elsewhere.com")? },
                { recipient: "bc2", domain: Url.Host.from_str("example.com")? },
            ],
        }),
        label: [Text("Send Feedback")],
    }) |> render_inline()
    actual == expected
}
expect "<a href=\"../one%20more/two.html\">The Link</a>"
    == Link({
        url: Relative({ path: ["..", "one more", "two.html"] }),
        label: [Text("The Link")],
    }) |> render_inline()
expect "<a href=\"#id-list-heading\">About</a>"
    == Link({
        url: Relative({ fragment: Ok("id-list-heading") }),
        label: [Text("About")],
    }) |> render_inline()
expect "<a href=\"/about.html\">About</a>"
    == Link({
        url: Relative({ root: True, path: ["about.html"] }),
        label: [Text("About")],
    }) |> render_inline()
expect {
    expected = "<a href=\"javascript%3Aalert%281%29\">The Link</a>"
    actual = Link({
        url: Relative({ path: ["javascript:alert(1)"] }),
        label: [Text("The Link")],
    }) |> render_inline()
    actual == expected
}
expect {
    expected = "<a href=\"../docs/comp.html#max\"><code>max : a, a -&gt; a where [is_gt : a, a -&gt; Bool]</code></a>"
    actual = Link({
        url: Relative({ path: ["..", "docs", "comp.html"], fragment: Ok("max") }),
        label: [Code({ attrs: Attrs.{}, content: "max : a, a -> a where [is_gt : a, a -> Bool]" })],
    }) |> render_inline()
    actual == expected
}

expect {
    expected = "<ul></ul>"
    actual = UnorderedList({ items: [] }) |> render_block()
    actual == expected
}
expect {
    expected = "<ul><li>first</li></ul>"
    actual = UnorderedList({ items: [Inlines([Text("first")])] }) |> render_block()
    actual == expected
}
expect {
    expected = "<ul><li>first&lt;second</li></ul>"
    actual = UnorderedList({ items: [Inlines([Text("first<second")])] }) |> render_block()
    actual == expected
}
expect {
    expected = "<ul id=\"1234\"><li>first</li></ul>"
    actual = UnorderedList({
        attrs: { id: "1234" },
        items: [Inlines([Text("first")])],
    }) |> render_block()
    actual == expected
}
expect {
    expected = "<ul><li>first</li><li>second</li><li>third</li></ul>"
    actual = UnorderedList({ items: [
        Inlines([Text("first")]),
        Inlines([Text("second")]),
        Inlines([Text("third")]),
    ] }) |> render_block()
    actual == expected
}

expect {
    expected = "<ol></ol>"
    actual = OrderedList({ items: [] }) |> render_block()
    actual == expected
}
expect {
    expected = "<ol><li>first</li></ol>"
    actual = OrderedList({ items: [Inlines([Text("first")])] }) |> render_block()
    actual == expected
}
expect {
    expected = "<ol><li>first&lt;second</li></ol>"
    actual = OrderedList({ items: [Inlines([Text("first<second")])] }) |> render_block()
    actual == expected
}
expect {
    expected = "<ol><li><a href=\"https://example.com/one%20mor%C3%A9%3F%23%2F%25/two.html\">first&lt;second</a></li></ol>"
    actual = OrderedList({
        items: [Inlines([Link({
            url: Http({ host: Url.Host.from_str("example.com")?, path: ["one moré?#/%", "two.html"] }),
            label: [ Text("first<second")],
        })])]
    }) |> render_block()
    actual == expected
}
expect {
    expected = "<ol><li><p>first</p></li></ol>"
    actual = OrderedList({
        items: [Blocks([Paragraph({ contents: [Text("first")] })])]
    }) |> render_block()
    actual == expected
}
expect {
    expected = "<ol id=\"1234\"><li>first</li></ol>"
    actual = OrderedList({
        attrs: { id: "1234" },
        items: [Inlines([Text("first")])],
    }) |> render_block()
    actual == expected
}
expect {
    expected = "<ol><li>first</li><li><ul><li>second 1</li><li>second 2</li></ul></li><li>third</li></ol>"
    actual = OrderedList({ items: [
        Inlines([Text("first")]),
        Blocks([UnorderedList({ items: [Inlines([Text("second 1")]), Inlines([Text("second 2")])] })]),
        Inlines([Text("third")]),
    ] }) |> render_block()
    actual == expected
}

expect {
    expected = "<img src=\"images/trees.jpg\" alt=\"Some trees &amp; bushes\">"
    actual = Image({
        url: Relative({ path: ["images", "trees.jpg"] }),
        alt: "Some trees & bushes",
    }) |> render_inline()
    actual == expected
}
expect {
    expected = "<img id=\"1234\" class=\"here there\" src=\"images/trees.jpg\" alt=\"Some trees &amp; bushes\">"
    actual = Image({
        attrs: { id: "1234", classes: ["here", "there"] },
        url: Relative({ path: ["images", "trees.jpg"] }),
        alt: "Some trees & bushes",
    }) |> render_inline()
    actual == expected
}
expect {
    expected = "<img src=\"images/trees.jpg\" alt=\"Some trees &amp; bushes\" width=\"128\" height=\"256\">"
    actual = Image({
        url: Relative({ path: ["images", "trees.jpg"] }),
        alt: "Some trees & bushes",
        width: 128,
        height: 256,
    }) |> render_inline()
    actual == expected
}
expect {
    expected = "<img src=\"images/trees.jpg\" alt=\"Some trees &amp; bushes\" height=\"256\">"
    actual = Image({
        url: Relative({ path: ["images", "trees.jpg"] }),
        alt: "Some trees & bushes",
        height: 256,
    }) |> render_inline()
    actual == expected
}
expect {
    expected = "<img src=\"images/trees.jpg\" alt=\"Some trees &amp; bushes\" width=\"128\">"
    actual = Image({
        url: Relative({ path: ["images", "trees.jpg"] }),
        alt: "Some trees & bushes",
        width: 128,
    }) |> render_inline()
    actual == expected
}

expect "<em>some text</em>" == Emphasis([Text("some text")]) |> render_inline()
expect {
    expected = "<div><h1>some text</h1></div>"
    actual = Div({ contents: [Heading({ level: H1, contents: [Text("some text")] }) ]})
        |> render_block()
    actual == expected
}
expect {
    expected = "<div id=\"1234\" class=\"one two\"><h1>some text</h1></div>"
    actual = Div({
        contents: [Heading({ level: H1, contents: [Text("some text")] })],
        attrs: { id: "1234", classes: ["one", "two"] },
    }) |> render_block()
    actual == expected
}
expect {
    expected = "<table><tbody><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></tbody></table>"
    table_spec = TableSpec.new({
        data: [
            [Inlines([Text("1")]), Inlines([Text("2")])],
            [Inlines([Text("3")]), Inlines([Text("4")])],
        ],
    })?
    actual = Table(table_spec) |> render_block()
    actual == expected
}
expect {
    expected = "<table><caption>a caption</caption><thead><tr><th scope=\"col\">one</th><th scope=\"col\">two</th></tr></thead></table>"
    table_spec = TableSpec.new({
        caption: "a caption",
        headers: [Inlines([Text("one")]), Inlines([Text("two")])],
        data: []
    })?
    actual = Table(table_spec) |> render_block()
    actual == expected
}
expect {
    expected = "<table><caption>a &lt;caption&gt;</caption><thead><tr><th scope=\"col\">one</th><th scope=\"col\">two</th></tr></thead><tbody><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></tbody></table>"
    table_spec = TableSpec.new({
        caption: "a <caption>",
        headers: [Inlines([Text("one")]), Inlines([Text("two")])],
        data: [
            [Inlines([Text("1")]), Inlines([Text("2")])],
            [Inlines([Text("3")]), Inlines([Text("4")])],
        ],
    })?
    actual = Table(table_spec) |> render_block()
    actual == expected
}
expect {
    expected = "<table><caption>all alignments</caption><thead><tr><th scope=\"col\">Default</th><th scope=\"col\" class=\"align-left\">Left</th><th scope=\"col\" class=\"align-center\">Center</th><th scope=\"col\" class=\"align-right\">Right</th><th scope=\"col\">None</th></tr></thead><tbody><tr><td>default</td><td class=\"align-left\">left</td><td class=\"align-center\">center</td><td class=\"align-right\">right</td><td>none</td></tr></tbody></table>"
    table_spec = TableSpec.new({
        caption: "all alignments",
        headers: [
            Inlines([Text("Default")]),
            Inlines([Text("Left")]),
            Inlines([Text("Center")]),
            Inlines([Text("Right")]),
            Inlines([Text("None")]),
        ],
        align: [Default, Left, Center, Right],
        data: [
            [
                Inlines([Text("default")]),
                Inlines([Text("left")]),
                Inlines([Text("center")]),
                Inlines([Text("right")]),
                Inlines([Text("none")]),
            ],
        ],
    })?
    actual = Table(table_spec) |> render_block()
    actual == expected
}
expect {
    expected = "<table id=\"1234\" class=\"here there\"><thead><tr><th scope=\"col\">one&lt;</th><th scope=\"col\">two</th></tr></thead><tbody><tr><td>1&gt;5</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></tbody></table>"
    table_spec = TableSpec.new({
        attrs: { id: "1234", classes: ["here", "there"] },
        headers: [Inlines([Text("one<")]), Inlines([Text("two")])],
        data: [
            [Inlines([Text("1>5")]), Inlines([Text("2")])],
            [Inlines([Text("3")]), Inlines([Text("4")])],
        ],
    })?
    table = Table(table_spec)
    actual = table |> render_block()
    actual == expected
}
expect Err(RaggedRows) == TableSpec.new({
    data: [
        [Inlines([Text("1>5")])],
        [Inlines([Text("3")]), Inlines([Text("4")])],
    ]
})

expect "<table></table>" == TableSpec.new({ data: [] })? |> Table |> render_block
expect "<table></table>" == TableSpec.{
    attrs: Attrs.{},
    caption: "",
    headers: [],
    align: [],
    data: [],
} |> Table |> render_block
expect {
    expected = "<table><tbody><tr><td><p>some text</p></td></tr></tbody></table>"
    p = Paragraph(InlinesSpec.{ contents: [Text("some text")] })
    bs = Blocks([p])
    ts = TableSpec.new(TableConfig.{ data: [
        [bs],
    ] })?
    actual = Table(ts) |> render_block()
    actual == expected
}

expect "<blockquote></blockquote>" == BlocksSpec.{ contents: [] } |> BlockQuote |> render_block
expect "<code></code>" == { attrs: Attrs.{}, content: "" } |> Code |> render_inline
expect "<code>1234 hello()</code>"
    == { attrs: Attrs.{}, content: "1234 hello()" } |> Code |> render_inline
expect "<code class=\"roc\">1234 hello()</code>"
    == {
        attrs: Attrs.{ classes: ["roc"] },
        content: "1234 hello()",
    }|> Code |> render_inline

expect "<pre class=\"some\"></pre>" == InlinesSpec.{
    attrs: Attrs.{
        classes: ["some"],
    },
    contents: [],
} |> Pre |> render_block
expect "<pre><code class=\"roc\">1234 hello()</code></pre>"
    == InlinesSpec.{ contents: [
        {
            attrs: Attrs.{ classes: ["roc"] },
            content: "1234 hello()",
        } |> Code
    ] } |> Pre |> render_block

expect "<hr>" == Attrs.{} |> Rule |> render_block
expect "<hr class=\"pretty\">" == Attrs.{ classes: ["pretty"] } |> Rule |> render_block
expect "<div><p>A</p><hr><p>B</p></div>"
    == Div(BlocksSpec.{ contents: [
        Paragraph(InlinesSpec.{ contents: [Text("A")] }),
        Rule(Attrs.{}),
        Paragraph(InlinesSpec.{ contents: [Text("B")] }),
    ] }) |> render_block

expect "<span></span>" == InlinesSpec.{ contents: [] } |> Span |> render_inline
expect "<span class=\"outer\"><span class=\"inner\">some text</span></span>"
    == InlinesSpec.{
        attrs: Attrs.{ classes: ["outer"] },
        contents: [
            Span(InlinesSpec.{
                attrs: Attrs.{ classes: ["inner"] },
                contents: [Text("some text")],
            })
        ],
    } |> Span |> render_inline
