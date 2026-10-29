Html := {}.{
    Doc := { body: List(Content) ?? [] }.{
        empty : Doc
        empty = Doc.{}

        with_body : Doc, List(Content) -> Doc
        with_body = |doc, body| { ..doc, body }

        render : Doc -> Str
        render = |doc|
            "<!DOCTYPE html>\n<html>${render_body(doc.body)}</html>"
    }

    Content := [
        Text(Str),
        Strong(List(Content)),
        Heading({ level: U64, content: List(Content) }),
        Paragraph(List(Content)),
    ]
}

render_body : List(Content) -> Str
render_body = |body|
    if body.is_empty()
        ""
    else
        "<body>${body.map(render_content) |> Str.join_with("")}</body>"

render_content : Content -> Str
render_content = |c|
    match c {
        Text(s) => s.replace_each("&", "&amp;").replace_each(">", "&gt;").replace_each("<", "&lt;")
        Strong(content) => "<strong>${content.map(render_content) |> Str.join_with("")}</strong>"
        Heading({level, content}) =>
            "<h${level.to_str()}>${content.map(render_content) |> Str.join_with("")}</h${level.to_str()}>"
        Paragraph(content) => "<p>${content.map(render_content) |> Str.join_with("")}</p>"
    }

expect "a &gt; b &amp; c &lt; d" == Text("a > b & c < d") |> render_content()

expect "<h1>Shopping List</h1>"
    == Heading({ level: 1, content: [Text("Shopping List")] })
        |> render_content()
expect "<p>Shopping List</p>"
    == Paragraph([Text("Shopping List")])
        |> render_content()
expect "<p>Shopping <strong>List</strong></p>"
    == Paragraph([Text("Shopping "), Strong([Text("List")])])
        |> render_content()

expect \\<!DOCTYPE html>
        \\<html></html>
    == Doc.empty.render()

expect {
    expected = \\<!DOCTYPE html>
               \\<html><body><h1>Shopping List</h1></body></html>
    actual = Doc.empty
        .with_body([Heading({ level: 1, content: [Text("Shopping List")] })])
        .render()
    actual == expected
}
