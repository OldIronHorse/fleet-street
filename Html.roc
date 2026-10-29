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
        Heading({ level: U64, content: List(Inline) }),
        Paragraph(List(Inline)),
    ]
}

render_body : List(Block) -> Str
render_body = |body|
    if body.is_empty()
        ""
    else
        "<body>${body.map(render_block) |> Str.join_with("")}</body>"

render_block : Block -> Str
render_block = |b|
    match b {
        Heading({level, content}) =>
            "<h${level.to_str()}>${content.map(render_inline) |> Str.join_with("")}</h${level.to_str()}>"
        Paragraph(content) => "<p>${content.map(render_inline) |> Str.join_with("")}</p>"
    }

render_inline : Inline -> Str
render_inline = |c|
    match c {
        Text(s) => s.replace_each("&", "&amp;").replace_each(">", "&gt;").replace_each("<", "&lt;")
        Strong(content) => "<strong>${content.map(render_inline) |> Str.join_with("")}</strong>"
    }

expect "a &gt; b &amp; c &lt; d" == Text("a > b & c < d") |> render_inline()

expect "<h1>Shopping List</h1>"
    == Heading({ level: 1, content: [Text("Shopping List")] })
        |> render_block()
expect "<p>Shopping List</p>"
    == Paragraph([Text("Shopping List")])
        |> render_block()
expect "<p>Shopping <strong>List</strong></p>"
    == Paragraph([Text("Shopping "), Strong([Text("List")])])
        |> render_block()

expect \\<!DOCTYPE html>
        \\<html lang=\"en\"><head><title>1234</title></head></html>
    == Doc.empty({ title: "1234" }).render()

expect {
    expected = \\<!DOCTYPE html>
               \\<html lang="en"><head><title>abcde</title></head><body><h1>Shopping List</h1></body></html>
    actual = Doc.empty({ title: "abcde" })
        .with_body([Heading({ level: 1, content: [Text("Shopping List")] })])
        .render()
    actual == expected
}
