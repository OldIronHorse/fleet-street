import Url

Html := {}.{
    Doc := {
        lang: Str ?? "en",
        head: Head,
        body: List(Block) ?? []
    }.{
        Head := { title: Str }.{
            render : Head -> Str
            render = |h|
                "<head><meta charset=\"utf-8\"><title>${escape(h.title)}</title></head>"
        }

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

    Inline := [
        Text(Str),
        Strong(List(Inline)),
        Emphasis(List(Inline)),
        Link({ label: List(LinkContent), url: Url }),
        Image(ImageSpec),
    ]

    LinkContent := [
        Text(Str),
        Strong(List(LinkContent)),
        Emphasis(List(LinkContent)),
        Image(ImageSpec),
    ].{
        to_inline : LinkContent -> Inline
        to_inline = |c|
            match c {
                Text(s) => Text(s)
                Strong(cs) => Strong(cs.map(to_inline))
                Emphasis(cs) => Emphasis(cs.map(to_inline))
                Image(s) => Image(s)
            }
    }

    Block := [
        Heading(HeadingSpec),
        Paragraph(ParagraphSpec),
        UnorderedList(ListSpec),
        OrderedList(ListSpec),
        Div(DivSpec),
        Table(TableSpec),
    ]

    TableConfig := {
        attrs: Attrs ?? {},
        caption: Str ?? "",
        headers: List(Content) ?? [],
        data: List(List(Content)),
    }

    TableSpec :: {
        attrs: Attrs,
        caption: Str,
        headers: List(Content),
        data: List(List(Content)),
    }.{
        new : TableConfig -> Try(TableSpec, [RaggedRows])
        new = |tc| {
            header_count = tc.headers.len()
            if tc.data.any(|row| row.len() > header_count)
                Err(RaggedRows)
            else
                Ok({ attrs: tc.attrs, caption: tc.caption, headers: tc.headers, data: tc.data })
        }
    }

    DivSpec := {
        attrs: Attrs ?? {},
        children: List(Block),
    }

    ImageSpec := {
        attrs:  Attrs ?? {},
        url: Url,
        alt: Str,
        height: U64 ?? 0,
        width: U64 ?? 0,
    }

    ListSpec := {
        attrs: Attrs ?? {},
        items: List(Content),
    }

    Content := [
        Inlines(List(Inline)),
        Blocks(List(Block)),
    ]

    Attrs := {
        id: Str ?? "",
        classes: List(Str) ?? [],
    }

    HeadingSpec := {
        attrs: Attrs ?? {},
        level: Level,
        children: List(Inline)
    }
    ParagraphSpec := {
        attrs: Attrs ?? {},
        children: List(Inline)
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
    }
}

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
    }

render_inlines : List(Inline) -> Str
render_inlines = |content|
    content.map(render_inline)
        |> Str.join_with("")

render_contents : List(Content) -> Str
render_contents = |items|
    items.map(render_content)
        |> Str.join_with("")

render_list_contents : List(Content) -> Str
render_list_contents = |items|
    items.map(render_list_content)
        |> Str.join_with("")

render_list_content : Content -> Str
render_list_content = |items| "<li>${render_content(items)}</li>"

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
            "<h${hs.level.to_str()}${render_attrs(hs.attrs)}>${render_inlines(hs.children)}</h${hs.level.to_str()}>"
        Paragraph(ps) =>
            "<p${render_attrs(ps.attrs)}>${render_inlines(ps.children)}</p>"
        UnorderedList(ls) =>
            "<ul${render_attrs(ls.attrs)}>${render_list_contents(ls.items)}</ul>"
        OrderedList(ls) =>
            "<ol${render_attrs(ls.attrs)}>${render_list_contents(ls.items)}</ol>"
        Div(ds) =>
            "<div${render_attrs(ds.attrs)}>${ds.children.map(render_block) |> Str.join_with("")}</div>"
        Table(ts) => {
            caption = if ts.caption.is_empty() "" else "<caption>${escape(ts.caption)}</caption>"
            hs = ts.headers.map(|h| "<th>${render_content(h)}</th>")
            rows = ts.data.map(render_table_row)
            "<table${render_attrs(ts.attrs)}>${caption}${if hs.is_empty() "" else "<thead><tr>".concat(hs |> Str.join_with("")).concat("</tr></thead>")}<tbody>${rows |> Str.join_with("")}</tbody></table>"
        }
    }

render_table_row : List(Content) -> Str
render_table_row = |cells|
    "<tr>${cells.map(|c| "<td>".concat(render_content(c)).concat("</td>")) |> Str.join_with("")}</tr>"

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
    == Heading(HeadingSpec.{ level: H1, children: [Text("Shopping List")] })
        |> render_block()
expect "<h1 id=\"id-list-heading\">Shopping List</h1>"
    == Heading(HeadingSpec.{
        attrs: { id: "id-list-heading" },
        level: H1,
        children: [Text("Shopping List")]
    }) |> render_block()
expect "<p></p>" == Paragraph(ParagraphSpec.{ children: [] }) |> render_block()
expect "<p>Shopping List</p>"
    == Paragraph(ParagraphSpec.{ children: [Text("Shopping List")] })
        |> render_block()
expect "<p class=\"todo\">Shopping List</p>"
    == Paragraph(ParagraphSpec.{
        attrs: { classes: ["todo"] },
        children: [Text("Shopping List")]
    }) |> render_block()
expect "<p>Shopping List</p>"
    == Paragraph(ParagraphSpec.{
        attrs: { classes: [""] },
        children: [Text("Shopping List")]
    }) |> render_block()
expect "<p class=\"todo b&quot;ox\">Shopping List</p>"
    == Paragraph(ParagraphSpec.{
        attrs: { classes: ["todo", "b\"ox"] },
        children: [Text("Shopping List")]
    }) |> render_block()
expect {
    expected = "<p id=\"id-list-&quot;heading&quot;\" class=\"todo\">Shopping List</p>"
    actual = Paragraph(
        ParagraphSpec.{
            attrs: {
                id: "id-list-\"heading\"",
                classes: ["todo"],
            },
            children: [Text("Shopping List")]
        }) |> render_block()
    actual == expected
}
expect "<p>Shopping <strong>List</strong></p>"
    == Paragraph(ParagraphSpec.{ children: [Text("Shopping "), Strong([Text("List")])] })
        |> render_block()
expect "<p>Shopping <strong><strong>List</strong></strong></p>"
    == Paragraph(ParagraphSpec.{ children: [Text("Shopping "), Strong([Strong([Text("List")])])] })
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
        .with_body([Heading({ level: H1, children: [Text("Shopping List")] })])
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
        Paragraph({ children: [Text("first")] }),
        Paragraph({ children: [Text("second")] }),
        Paragraph({ children: [Text("third")] }),
    ] |> render_body()

expect "<a href=\"https://example.com/one%20mor%C3%A9%3F%23%2F%25/two.html\">The Link</a>"
    == Link({
        url: Http({ host: Url.Host.from_str("example.com")?, path: ["one moré?#/%", "two.html"] }),
        label: [Text("The Link")],
    }) |> render_inline()
expect {
    expected ="<a href=\"mailto:someone@example.com?subject=Feedback%20Request%20%26%20Link&amp;body=Please%20provide%20feedback.&amp;cc=cc1@elsewhere.com&amp;cc=cc2@example.com&amp;bcc=bc1@elsewhere.com&amp;bcc=bc2@example.com\">Send Feedback</a>"
    actual = Link({
        url: MailTo({
            to: { recipient: "someone", domain: Url.Host.from_str("example.com")? },
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
    expected = "<ol><li><p>first</p></li></ol>"
    actual = OrderedList({
        items: [Blocks([Paragraph({ children: [Text("first")] })])]
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

expect "<em>some text</em>" == Emphasis([Text("some text")]) |> render_inline()
expect {
    expected = "<div><h1>some text</h1></div>"
    actual = Div({ children: [Heading({ level: H1, children: [Text("some text")] }) ]})
        |> render_block()
    actual == expected
}
expect {
    expected = "<div id=\"1234\" class=\"one two\"><h1>some text</h1></div>"
    actual = Div({
        children: [Heading({ level: H1, children: [Text("some text")] })],
        attrs: { id: "1234", classes: ["one", "two"] },
    }) |> render_block()
    actual == expected
}
expect {
    expected = "<table><caption>a caption</caption><thead><tr><th>one</th><th>two</th></tr></thead><tbody><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></tbody></table>"
    table_spec = TableSpec.new({
        caption: "a caption",
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
    expected = "<table id=\"1234\" class=\"here there\"><thead><tr><th>one</th><th>two</th></tr></thead><tbody><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></tbody></table>"
    table_spec = TableSpec.new({
        attrs: { id: "1234", classes: ["here", "there"] },
        headers: [Inlines([Text("one")]), Inlines([Text("two")])],
        data: [
            [Inlines([Text("1")]), Inlines([Text("2")])],
            [Inlines([Text("3")]), Inlines([Text("4")])],
        ],
    })?
    table = Table(table_spec)
    actual = table |> render_block()
    actual == expected
}
