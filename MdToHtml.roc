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
                attrs: Html.Attrs.{ classes: ["language-${cs.info.split_first(" ").ok_or({ before: "", after: "" }).before}"] },
                contents: [Text(cs.pre)],
            }),
        ] })
        ThematicBreak => Rule(Html.Attrs.{})
        Table(t) => {
            match Html.TableSpec.new(Html.TableConfig.{
                headers: t.header.map(|header| Inlines(header.map(|c| inline(c)))),
                align: t.align.map(table_align),
                data: t.rows.map(|row| row.map(|cell| Inlines(cell.map(|c| inline(c))))),
            }) {
                Ok(ts) => Table(ts)
                Err(_) => { crash "Unreachable: MdToHtml.block"} # the markdown parser pads/truncates rows as necessary
            }
        }
        b => {
            dbg b
            Paragraph(Html.ParagraphSpec.{ children: [Text("Todo")] })
        }
    }

table_align : Markdown.Alignment -> Html.Alignment
table_align = |a|
    match a {
        Default => Left
        Left => Left
        Center => Center
        Right => Right
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
                Code(Html.CodeSpec.{
                    attrs: Html.Attrs.{ classes: ["language-roc"] },
                    contents: [
                        Text(\\some_fn : Str, U64 -> Str
                             \\some_fn = |s, n|
                             \\    s.repeat(n)
                             \\
                        )
                    ]
                }),
            ] }),
        ],
    }
    md = \\```roc title="x"
         \\some_fn : Str, U64 -> Str
         \\some_fn = |s, n|
         \\    s.repeat(n)
         \\```
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            Rule(Html.Attrs.{}),
            Rule(Html.Attrs.{}),
            Rule(Html.Attrs.{}),
        ],
    }
    md = \\***
         \\---
         \\___
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            Table(Html.TableSpec.new(Html.TableConfig.{
                headers: [Inlines([Text("Item")]), Inlines([Text("Qty")])],
                align: [Left, Right],
                data: [
                    [Inlines([Text("pear")]), Inlines([Text("2")])],
                ],
            })?),
        ],
    }
    md = \\| Item | Qty |
         \\| :--- | --: |
         \\| pear | 2 |
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            Table(Html.TableSpec.new(Html.TableConfig.{
                headers: [Inlines([Text("Item")]), Inlines([Text("Qty")])],
                align: [Left, Right],
                data: [
                    [Inlines([Text("pear")]), Inlines([Text("2")])],
                ],
            })?),
        ],
    }
    md = \\| Item | Qty |
         \\| :--- | --: |
         \\| pear | 2 | 3 |
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Doc.Head.{ title: "My Shopping List" },
        body: [
            Table(Html.TableSpec.new(Html.TableConfig.{
                headers: [Inlines([Text("Item")]), Inlines([Text("Qty")])],
                align: [Left, Right],
                data: [
                    [Inlines([Text("pear")]), Inlines([Text("")])],
                ],
            })?),
        ],
    }
    md = \\| Item | Qty |
         \\| :--- | --: |
         \\| pear |
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List")
    actual.render() == expected.render()
}
