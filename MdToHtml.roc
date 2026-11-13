import parser.Markdown
import Html

MdToHtml := {}.{
    doc : List(Markdown), Str -> Html.Doc
    doc = |blocks, title| {
        blocks.fold(Html.Doc.with_head(Html.Doc.Head.{ title }), add_block)
    }
}

inline : Markdown.Inline -> Html.Inline
inline = |md|
    match md {
        Text(s) => Text(s)
        Strong(ss) => Strong(ss.map(inline))
        Emphasis(ss) => Emphasis(ss.map(inline))
        _ => { crash "not implemented" }
    }

add_block : Html.Doc, Markdown -> Html.Doc
add_block = |doc, b|
    match b {
        Frontmatter(_) => doc
        _ => doc.with_body(doc.body.append(block(b)))
    }

block : Markdown -> Html.Block
block = |md|
    match md {
        Heading(h) => Heading(Html.HeadingSpec.{ level: heading_level(h.level), children: h.content.map(inline) })
        Paragraph(cs) => Paragraph(Html.ParagraphSpec.{ children: cs.map(inline) })
        ListBlock(lb) => {
            list_spec = Html.ListSpec.{ items: lb.items.map(|i| i.blocks.map(block) |> Blocks) }
            match lb.kind {
                Unordered => UnorderedList(list_spec)
                Ordered(_) => OrderedList(list_spec)
            }
        }
        Blockquote(bs) => BlockQuote(Html.BlockQuoteSpec.{ contents: bs.map(block) })
        Code(cs) => Pre(Html.ParagraphSpec.{ children: [
            Code(Html.CodeSpec.{
                language: cs.info,
                contents: [Text(cs.pre)],
            }),
        ] })
        b => {
            dbg b
            Paragraph(Html.ParagraphSpec.{ children: [Text("Todo")] })
        }
    }

heading_level : Markdown.Level -> Html.Level
heading_level = |level|
    match level {
        One => H1
        Two => H2
        Three => H3
        Four => H4
        Five => H5
        Six => H6
    }

expect {
    expected = [Text("Some text.")]
    actual = Markdown.parse_inlines("Some text.").map(inline)
    actual == expected
}

expect {
    expected = [Text("Some "), Strong([Text("text")]), Text(".")]
    actual = Markdown.parse_inlines("Some **text**.").map(inline)
    actual == expected
}

expect {
    expected = [Text("Some "), Emphasis([Text("text")]), Text(".")]
    actual = Markdown.parse_inlines("Some *text*.").map(inline)
    actual == expected
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            Heading(Html.HeadingSpec.{ level: H1, children: [Text("Shopping")] }),
        ],
    }
    md = "# Shopping"
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            Heading(Html.HeadingSpec.{ level: H1, children: [Text("Shopping")] }),
            Paragraph(Html.ParagraphSpec.{ children: [Text("Buy these "), Strong([Text("today")]), Text(":")] }),
        ],
    }
    md = "# Shopping\n\nBuy these **today**:"
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            UnorderedList(Html.ListSpec.{ items: [
                Blocks([Paragraph(Html.ParagraphSpec.{ children: [Text("apples")] })]),
                Blocks([Paragraph(Html.ParagraphSpec.{ children: [Text("pears")] })]),
            ] }),
        ],
    }
    md = \\- apples
         \\- pears
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            BlockQuote(Html.BlockQuoteSpec.{ contents: [
                Paragraph(Html.ParagraphSpec.{ children: [Text("apples\npears")] }),
            ] }),
        ],
    }
    md = \\> apples
         \\> pears
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            Pre(Html.ParagraphSpec.{ children: [
                Code(Html.CodeSpec.{ language: "roc", contents: [Text(\\some_fn : Str, U64 -> Str
                                                                      \\some_fn = |s, n|
                                                                      \\    s.repeat(n)
                                                                      \\
                                                                )]
                }),
            ] }),
        ],
    }
    md = \\```roc
         \\some_fn : Str, U64 -> Str
         \\some_fn = |s, n|
         \\    s.repeat(n)
         \\```
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    dbg actual
    dbg expected
    actual.render() == expected.render()
}
