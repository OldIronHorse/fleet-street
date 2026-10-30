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
        Link({ label: Inline, url: Url }),
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

    Url := [
        Http(HttpSpec),
        MailTo(MailToSpec),
        Relative(List(Str)),
    ].{
        to_str : Url -> Str
        to_str = |url|
            match url {
                Http(hs) => {
                    port = hs.port.map_ok(|p| ":${p.to_str()}").ok_or("")
                    query =
                        if hs.query.is_empty()
                            "" 
                        else
                            Str.join_with(hs.query.map(|p| p.url_encode()), "&").with_prefix("?")
                    fragment = hs.fragment.map_ok(|f| "#${f}").ok_or("")
                    "${hs.scheme.to_str()}://${url_encode(hs.host)}${port}/${hs.path.map(url_encode) |> Str.join_with("/")}${query}${fragment}"
                }
                MailTo(mt) => {
                    subject = if mt.subject.is_empty() [] else ["subject=${url_encode(mt.subject)}"]
                    body = if mt.body.is_empty() [] else ["body=${url_encode(mt.body)}"]
                    cc = mt.cc.map(|a| "cc=${a.url_encode()}")
                    bcc = mt.bcc.map(|a| "bcc=${a.url_encode()}")
                    args = [subject, body, cc, bcc]
                        .join()
                        |> Str.join_with("&")
                    "mailto:${mt.to.url_encode()}${if args.is_empty() "" else args.with_prefix("?")}"
                }
                Relative(path) =>
                    path.map(url_encode) |> Str.join_with("/")
            }
    }

    HttpSpec := {
        host: Str,
        port: Try(U64, [None]) ?? Err(None),
        path: List(Str),
        scheme: Scheme ?? Https
        query: List(QueryParam) ?? [],
        fragment: Try(Str, [None]) ?? Err(None),
    }

    Scheme := [Http, Https].{
        to_str : Scheme -> Str
        to_str = |s|
            match s {
                Http => "http"
                Https => "https"
            }
    }

    QueryParam := { key: Str, value: Str }.{
        url_encode : QueryParam -> Str
        url_encode = |qp|
            "${url_encode(qp.key)}=${url_encode(qp.value)}"
    }

    Address := { recipient: Str, domain: Str }.{
        url_encode : Address -> Str
        url_encode = |addr|
            "${url_encode(addr.recipient)}@${url_encode(addr.domain)}"

    }
    MailToSpec := {
        to: Address,
        subject: Str ?? "",
        body: Str ?? "",
        cc: List(Address) ?? [],
        bcc: List(Address)?? [],
    }
}

url_encode : Str -> Str
url_encode = |s|
    s.replace_each(" ", "%20")
        .replace_each("\"", "%22")
        .replace_each("/", "%2F")
        .replace_each("?", "%2F")
        .replace_each("#", "%3F")

render_body : List(Block) -> Str
render_body = |body|
    "<body>${body.map(render_block) |> Str.join_with("")}</body>"

render_inline : Inline -> Str
render_inline = |c|
    match c {
        Text(s) => escape(s)
        Strong(content) => "<strong>${render_inlines(content)}</strong>"
        Link({ url, label }) =>
            "<a href=\"${url.to_str()}\">${label |> render_inline()}</a>"
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

expect "http://example.com/one/two/three/four.html"
    == Http({ scheme: Http, host: "example.com", path: ["one", "two", "three", "four.html"] })
        |> Url.to_str()

expect "https://example.com/one/two/three/four.html"
    == Http({ host: "example.com", path: ["one", "two", "three", "four.html"] })
        |> Url.to_str()

expect "https://example.com/one/two/three/four.html#section1"
    == Http({
        host: "example.com",
        path: ["one", "two", "three", "four.html"],
        fragment: Ok("section1")
    }) |> Url.to_str()

expect "https://example.com:8000/one/two/three/four.html"
    == Http({ host: "example.com", port: Ok(8000), path: ["one", "two", "three", "four.html"] })
        |> Url.to_str()

expect "https://example.com/one/two/three/four.html?name=Joe%20Bloggs&age=45"
    == Http({
        host: "example.com", 
        path: ["one", "two", "three", "four.html"],
        query: [{ key: "name", value: "Joe Bloggs" }, { key: "age", value: "45" }],
    }) |> Url.to_str()

expect "<a href=\"https://example.com/one%20more/two.html\">The Link</a>"
    ==  Link({
        url: Http({ host: "example.com", path: ["one more", "two.html"] }),
        label: Text("The Link"),
    }) |> render_inline()
expect {
    expected ="<a href=\"mailto:someone@example.com?subject=Feedback%20Request&body=Please%20provide%20feedback.&cc=cc1@elsewhere.com&cc=cc2@example.com&bcc=bc1@elsewhere.com&bcc=bc2@example.com\">Send Feedback</a>"
    actual =  Link({
        url: MailTo({
            to: { recipient: "someone", domain: "example.com" },
            subject: "Feedback Request",
            body: "Please provide feedback.",
            cc: [
                { recipient: "cc1", domain: "elsewhere.com"},
                { recipient: "cc2", domain: "example.com" },
            ],
            bcc: [
                { recipient: "bc1", domain: "elsewhere.com"},
                { recipient: "bc2", domain: "example.com" },
            ],
        }),
        label: Text("Send Feedback"),
    }) |> render_inline()
    actual == expected
}
expect "<a href=\"../one%20more/two.html\">The Link</a>"
    ==  Link({
        url: Relative(["..", "one more", "two.html"]),
        label: Text("The Link"),
    }) |> render_inline()
