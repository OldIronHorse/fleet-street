import parser.Markdown
import Html

MdToHtml := {}.{
    inline : Markdown.Inline -> Html.Inline
    inline = |md|
        match md {
            Text(s) => Text(s)
            Emphasis(ss) => Strong(ss.map(inline))
            _ => { crash "not implemented" }
        }
}

expect {
    expected = [Text("Some text.")]
    actual = Markdown.parse_inlines("Some text.").map(MdToHtml.inline)
    actual == expected
}

expect {
    expected = [Text("Some "), Strong([Text("text")]), Text(".")]
    actual = Markdown.parse_inlines("Some *text*.").map(MdToHtml.inline)
    dbg actual
    dbg expected
    actual == expected
}
