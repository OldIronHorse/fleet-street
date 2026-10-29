Html := {}.{
    Doc := {
        lang: Str ?? "en",
        head: Head,
        body: List(Block) ?? []
    }.{
        Head := { title: Str }.{
            render : Head -> Str
            render = |h|
                "<head><title>${Text(h.title) |> render_inline()}</title></head>"
        }

        empty : Head -> Doc
        empty = |head|
            Doc.{ head }

        with_lang : Doc, Str -> Doc
        with_lang = |doc, lang| { ..doc, lang }

        with_body : Doc, List(Block) -> Doc
        with_body = |doc, body| { ..doc, body }

        render : Doc -> Str
        render = |doc|
            "<!DOCTYPE html>\n<html lang=\"${doc.lang}\">${doc.head.render()}${render_body(doc.body)}</html>"
    }

    Inline := [
        Text(Str),
        Strong(List(Inline)),
    ]

    Block := [
        Heading(HeadingSpec),
        Paragraph(ParagraphSpec),
    ]

    HeadingSpec := { level: Level, content: List(Inline) }
    ParagraphSpec := { attrs: List(Attr) ?? [], children: List(Inline)}

    Attr := [Class(Str), Id(Str)]

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
        Strong(content) => "<strong>${content.map(render_inline) |> Str.join_with("")}</strong>"
    }

escape : Str -> Str
escape = |s|
    s.replace_each("&", "&amp;").replace_each(">", "&gt;").replace_each("<", "&lt;")

render_block : Block -> Str
render_block = |b|
    match b {
        Heading(hs) =>
            "<h${hs.level.to_str()}>${hs.content.map(render_inline) |> Str.join_with("")}</h${hs.level.to_str()}>"
        Paragraph(ps) =>
            "<p${render_attrs(ps.attrs)}>${ps.children.map(render_inline) |> Str.join_with("")}</p>"
    }

render_attrs : List(Attr) -> Str
render_attrs = |attrs|
    if attrs.is_empty()
        ""
    else
        " ${attrs.map(render_attr) |> Str.join_with(" ")}"

render_attr : Attr -> Str
render_attr = |attr|
    match attr {
        Class(c) => "class=\"${escape(c)}\""
        Id(id) => "id=\"${escape(id)}\""
    }

expect "a &gt; b &amp; c &lt; d" == Text("a > b & c < d") |> render_inline()

expect "<h1>Shopping List</h1>"
    == Heading(HeadingSpec.{ level: H1, content: [Text("Shopping List")] })
        |> render_block()
expect "<p></p>" == Paragraph(ParagraphSpec.{ children: [] }) |> render_block()
expect "<p>Shopping List</p>"
    == Paragraph(ParagraphSpec.{ children: [Text("Shopping List")] })
        |> render_block()
expect "<p class=\"todo\">Shopping List</p>"
    == Paragraph(ParagraphSpec.{ children: [Text("Shopping List")], attrs: [Class("todo")] })
        |> render_block()
expect "<p>Shopping <strong>List</strong></p>"
    == Paragraph(ParagraphSpec.{ children: [Text("Shopping "), Strong([Text("List")])] })
        |> render_block()
expect "<p>Shopping <strong><strong>List</strong></strong></p>"
    == Paragraph(ParagraphSpec.{ children: [Text("Shopping "), Strong([Strong([Text("List")])])] })
        |> render_block()

expect \\<!DOCTYPE html>
        \\<html lang=\"en\"><head><title>1234</title></head><body></body></html>
    == Doc.empty({ title: "1234" }).render()

expect {
    expected = \\<!DOCTYPE html>
               \\<html lang="en"><head><title>abcde</title></head><body><h1>Shopping List</h1></body></html>
    actual = Doc.empty({ title: "abcde" })
        .with_body([Heading({ level: H1, content: [Text("Shopping List")] })])
        .render()
    actual == expected
}

expect "<body><p>first</p><p>second</p><p>third</p></body>"
    == [
        Paragraph({ children: [Text("first")] }),
        Paragraph({ children: [Text("second")] }),
        Paragraph({ children: [Text("third")] }),
    ] |> render_body()
