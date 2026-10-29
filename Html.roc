Html := {}.{
    Doc := {
        lang: Str ?? "en",
        head: Head,
        body: List(Block) ?? []
    }.{
        Head := { title: Str }.{
            render : Head -> Str
            render = |h|
                "<head><title>${escape(h.title)}</title></head>"
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
    ]

    Block := [
        Heading(HeadingSpec),
        Paragraph(ParagraphSpec),
    ]

    Attrs := {
        id: Str ?? "",
        classes: List(Str) ?? [],
    }

    HeadingSpec := {
        attrs: Attrs ?? {}
        level: Level,
        children: List(Inline)
    }
    ParagraphSpec := {
        attrs: Attrs ?? {}
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
    }

render_inlines : List(Inline) -> Str
render_inlines = |content|
    content.map(render_inline)
        |> Str.join_with("")

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
    }

render_attrs : Attrs -> Str
render_attrs = |attrs| {
    id_ = if attrs.id.is_empty() "" else " id=\"${escape(attrs.id)}\""
    class = match attrs.classes.drop_if(Str.is_empty) {
        [] => ""
        cs => " class=\"${cs.map(escape).drop_if(Str.is_empty) |> Str.join_with(" ")}\""
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
       \\<html lang="en"><head><title>12&lt;34</title></head><body></body></html>
    == Doc.with_head({ title: "12<34" })
        .render()
expect \\<!DOCTYPE html>
       \\<html lang="en&quot;x"><head><title>12&lt;34</title></head><body></body></html>
    == Doc.with_head({ title: "12<34" })
        .with_lang("en\"x")
        .render()
expect {
    expected = \\<!DOCTYPE html>
               \\<html lang="en"><head><title>abcde</title></head><body><h1>Shopping List</h1></body></html>
    actual = Doc.with_head({ title: "abcde" })
        .with_body([Heading({ level: H1, children: [Text("Shopping List")] })])
        .render()
    actual == expected
}

expect "<body><p>first</p><p>second</p><p>third</p></body>"
    == [
        Paragraph({ children: [Text("first")] }),
        Paragraph({ children: [Text("second")] }),
        Paragraph({ children: [Text("third")] }),
    ] |> render_body()
