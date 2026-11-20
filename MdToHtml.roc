import parser.Markdown
import Html
import Url

MdToHtml := {}.{
    doc : List(Markdown), Str, List(Url) -> Try(Html.Doc, _)
    doc = |blocks, title, css| {
        blocks.fold_try(Html.Doc.with_head(Html.Head.{ title, css }), add_block)
    }
}

inline : Markdown.Inline -> Try(Html.Inline, _)
inline = |md|
    match md {
        Text(s) => Text(s) |> Ok
        Strong(ss) => Strong(ss.map_try(inline)?) |> Ok
        Emphasis(ss) => Emphasis(ss.map_try(inline)?) |> Ok
        Link(spec) =>
            Link({
                label: spec.label.map_try(link_content)?,
                url: Url.Url.from_str(spec.target.href)?,
            }) |> Ok
        _ => { crash "inline: not implemented for ${Str.inspect(md)}" }
    }

link_content : Markdown.Inline -> Try(Html.LinkContent, _)
link_content = |md|
    match md {
        Text(s) => Text(s) |> Ok
        _ => { crash "link_content: not implemented for ${Str.inspect(md)}" }
    }

add_block : Html.Doc, Markdown -> Try(Html.Doc, _)
add_block = |doc, b|
    match b {
        Frontmatter(_) => doc |> Ok
        _ => doc.with_body(doc.body.append(block(b)?)) |> Ok
    }

block : Markdown -> Try(Html.Block, _)
block = |md|
    match md {
        Heading(h) => Heading(Html.HeadingSpec.{ level: heading_level(h.level), contents: h.content.map_try(inline)? }) |> Ok
        Paragraph(cs) => Paragraph(Html.InlinesSpec.{ contents: cs.map_try(inline)? }) |> Ok
        ListBlock(lb) => {
            list_spec = Html.ContentSpec.{
                items: lb.items.map_try(|i| i.blocks.map_try(block)? |> Blocks |> Ok)?
            }
            match lb.kind {
                Unordered => UnorderedList(list_spec)
                Ordered(_) => OrderedList(list_spec)
            } |> Ok
        }
        Blockquote(bs) => BlockQuote(Html.BlocksSpec.{ contents: bs.map_try(block)? }) |> Ok
        Code(cs) => {
            classes = if cs.info.is_empty() [] else ["language-${cs.info.split_first(" ").ok_or({ before: "", after: "" }).before}"]
            Pre(Html.InlinesSpec.{ contents: [
                Code({
                    attrs: Html.Attrs.{ classes },
                    content: cs.pre,
                }),
            ] }) |> Ok
        }
        ThematicBreak => Rule(Html.Attrs.{}) |> Ok
        Table(t) => {
            match Html.TableSpec.new(Html.TableConfig.{
                headers: t.header.map_try(|header| Inlines(header.map_try(inline)?) |> Ok)?,
                align: t.align.map(table_align),
                data: t.rows.map_try(|row| row.map_try(|cell| Inlines(cell.map_try(inline)?) |> Ok))?,
            }) {
                Ok(ts) => Table(ts) |> Ok
                Err(_) => { crash "Unreachable: MdToHtml.block"} # the markdown parser pads/truncates rows as necessary
            }
        }
        b => {
            dbg b
            Paragraph(Html.InlinesSpec.{ contents: [Text("Todo")] }) |> Ok
        }
    }

table_align : Markdown.Alignment -> Html.Alignment
table_align = |a|
    match a {
        Default => Default
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
    actual = Markdown.parse_inlines("Some text.").map_try(inline)?
    actual == expected
}

expect {
    expected = [Text("Some "), Strong([Text("text")]), Text(".")]
    actual = Markdown.parse_inlines("Some **text**.").map_try(inline)?
    actual == expected
}

expect {
    expected = [Text("Some "), Emphasis([Text("text")]), Text(".")]
    actual = Markdown.parse_inlines("Some *text*.").map_try(inline)?
    actual == expected
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Heading(Html.HeadingSpec.{ level: H1, contents: [Text("Shopping")] }),
        ],
    }
    md = "# Shopping"
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Heading(Html.HeadingSpec.{ level: H1, contents: [Text("Shopping")] }),
            Paragraph(Html.InlinesSpec.{ contents: [Text("Buy these "), Strong([Text("today")]), Text(":")] }),
        ],
    }
    md = "# Shopping\n\nBuy these **today**:"
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            UnorderedList(Html.ContentSpec.{ items: [
                Blocks([Paragraph(Html.InlinesSpec.{ contents: [Text("apples")] })]),
                Blocks([Paragraph(Html.InlinesSpec.{ contents: [Text("pears")] })]),
            ] }),
        ],
    }
    md = \\- apples
         \\- pears
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            BlockQuote(Html.BlocksSpec.{ contents: [
                Paragraph(Html.InlinesSpec.{ contents: [Text("apples\npears")] }),
            ] }),
        ],
    }
    md = \\> apples
         \\> pears
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Pre(Html.InlinesSpec.{ contents: [
                Code({
                    attrs: Html.Attrs.{ classes: ["language-roc"] },
                    content: \\some_fn : Str, U64 -> Str
                             \\some_fn = |s, n|
                             \\    s.repeat(n)
                             \\
                }),
            ] }),
        ],
    }
    md = \\```roc title="x"
         \\some_fn : Str, U64 -> Str
         \\some_fn = |s, n|
         \\    s.repeat(n)
         \\```
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Pre(Html.InlinesSpec.{ contents: [
                Code({
                    attrs: Html.Attrs.{},
                    content: \\some_fn : Str, U64 -> Str
                             \\some_fn = |s, n|
                             \\    s.repeat(n)
                             \\
                }),
            ] }),
        ],
    }
    md = \\```
         \\some_fn : Str, U64 -> Str
         \\some_fn = |s, n|
         \\    s.repeat(n)
         \\```
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Pre(Html.InlinesSpec.{ contents: [
                Code({
                    attrs: Html.Attrs.{ classes: ["language-roc"] },
                    content: \\[a](http://x.com)
                             \\<&>
                             \\
                }),
            ] }),
        ],
    }
    md = \\```roc title="x"
         \\[a](http://x.com)
         \\<&>
         \\```
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Rule(Html.Attrs.{}),
            Rule(Html.Attrs.{}),
            Rule(Html.Attrs.{}),
        ],
    }
    md = \\***
         \\---
         \\___
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Table(Html.TableSpec.new(Html.TableConfig.{
                headers: [Inlines([Text("Item")]), Inlines([Text("Qty")]), Inlines([Text("Price")])],
                align: [Left, Default, Center],
                data: [
                    [Inlines([Text("pear")]), Inlines([Text("2")]), Inlines([Text("£0.39")])],
                ],
            })?),
        ],
    }
    md = \\| Item | Qty | Price |
         \\| :--- | --- | :---: |
         \\| pear | 2 | £0.39 |
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
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
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
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
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
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
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    actual.render() == expected.render()
}

expect {
    expected = Html.Doc.{
        lang: "en",
        head: Html.Head.{ title: "My Shopping List" },
        body: [
            Paragraph(Html.InlinesSpec.{ contents: [
                Link({
                    label: [Text("inline-style link")],
                    url: Url.Http(Url.HttpSpec.{
                        scheme: Https,
                        host: Url.Host.from_str("example.com")?,
                    })
                }),
            ]}),
        ],
    }
    md = \\[inline-style link](https://example.com)
    actual = md |> Markdown.parse_str() |> MdToHtml.doc("My Shopping List", [])?
    assert_equal(expected.render(), actual.render())
}

assert_equal = |expected, actual|
    if actual == expected
        True
    else {
        dbg { expected, actual }
        False
    }
