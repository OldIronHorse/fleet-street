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
        Link({ label: List(LinkContent), url: Url }),
    ]

    LinkContent := [
        Text(Str),
        Strong(List(LinkContent)),
    ].{
        to_inline : LinkContent -> Inline
        to_inline = |c|
            match c {
                Text(s) => Text(s)
                Strong(cs) => Strong(cs.map(to_inline))
            }
    }

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
                    fragment = hs.fragment.map_ok(|f| "#${url_encode(f)}").ok_or("")
                    "${hs.scheme.to_str()}://${hs.host.to_str()}${port}/${hs.path.map(url_encode) |> Str.join_with("/")}${query}${fragment}"
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

    Host :: { name: Str }.{
        whitelist = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"
            .to_utf8()
            |> Set.from_list()

        from_str : Str -> Try(Host, [InvalidHost])
        from_str = |host|
            match host.to_utf8()
                .fold_try({total: 0, label: 0, dash: False}, |state, c|
                    match c {
                        '.' if state.dash or state.label == 0 => Err(InvalidHost)
                        '.' => Ok({ total: state.total + 1, label: 0, dash: False })
                        '-' if state.label == 0 => Err(InvalidHost)
                        _ if whitelist.contains(c) and state.label < 63 and state.total < 253 =>
                            Ok({ total: state.total + 1, label: state.label + 1, dash: c == '-' })
                        _ => Err(InvalidHost)
                    }) {
                Ok(state) if state.total > 0 and state.label > 0  and state.dash.not() =>
                    Ok({ name: host.with_ascii_lowercased() })
                _ => Err(InvalidHost)
            }

        to_str : Host -> Str
        to_str = |host| host.name

        is_eq : Host, Host -> Bool
        is_eq = |this, that|
            this.name == that.name
    }

    HttpSpec := {
        host: Host,
        port: Try(U16, [None]) ?? Err(None),
        path: List(Str) ?? [],
        scheme: Scheme ?? Https,
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

url_whitelist = "-._~abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    .to_utf8()
    |> Set.from_list()

url_encode : Str -> Str
url_encode = |s|
    s.to_utf8()
        .map(|c|
            if url_whitelist.contains(c)
                Str.from_utf8_lossy([c])
            else
                "%${as_hex(c)}")
        |> Str.join_with("")

render_body : List(Block) -> Str
render_body = |body|
    "<body>${body.map(render_block) |> Str.join_with("")}</body>"

render_inline : Inline -> Str
render_inline = |c|
    match c {
        Text(s) => escape(s)
        Strong(content) => "<strong>${render_inlines(content)}</strong>"
        Link({ url, label }) =>
            "<a href=\"${escape(url.to_str())}\">${label.map(|l| l.to_inline()) |> render_inlines()}</a>"
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

hex_digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"]
hex_digit = |n| hex_digits.get(n.to_u64()).ok_or("0")

as_hex : U8 -> Str
as_hex = |n| "${n.div_by(16) |> hex_digit()}${n.rem_by(16) |> hex_digit()}"

expect "00" == as_hex(0)
expect "0A" == as_hex(10)
expect "0F" == as_hex(15)
expect "10" == as_hex(16)
expect "FF" == as_hex(255)

expect "%0A" == url_encode("\n")

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
    == Http({ scheme: Http, host: Host.from_str("example.com")?, path: ["one", "two", "three", "four.html"] })
        |> Url.to_str()

expect "https://example.com/"
    == Http({ host: Host.from_str("example.com")? })
        |> Url.to_str()

expect "https://example.com/one/two/three/four.html"
    == Http({ host: Host.from_str("example.com")?, path: ["one", "two", "three", "four.html"] })
        |> Url.to_str()

expect "https://example.com/one/two/three/four.html#section%201"
    == Http({
        host: Host.from_str("example.com")?,
        path: ["one", "two", "three", "four.html"],
        fragment: Ok("section 1")
    }) |> Url.to_str()

expect "https://example.com:8000/one/two/three/four.html"
    == Http({
        host: Host.from_str("example.com")?,
        port: Ok(8000),
        path: ["one", "two", "three", "four.html"]
    }) |> Url.to_str()

expect "https://example.com/one/two/three/four.html?name=Joe%20Bl%26ggs&age=4%235"
    == Http({
        host: Host.from_str("example.com")?,
        path: ["one", "two", "three", "four.html"],
        query: [{ key: "name", value: "Joe Bl&ggs" }, { key: "age", value: "4#5" }],
    }) |> Url.to_str()

expect "<a href=\"https://example.com/one%20mor%C3%A9%3F%23%2F%25/two.html\">The Link</a>"
    == Link({
        url: Http({ host: Host.from_str("example.com")?, path: ["one moré?#/%", "two.html"] }),
        label: [Text("The Link")],
    }) |> render_inline()
expect {
    expected ="<a href=\"mailto:someone@example.com?subject=Feedback%20Request%20%26%20Link&amp;body=Please%20provide%20feedback.&amp;cc=cc1@elsewhere.com&amp;cc=cc2@example.com&amp;bcc=bc1@elsewhere.com&amp;bcc=bc2@example.com\">Send Feedback</a>"
    actual = Link({
        url: MailTo({
            to: { recipient: "someone", domain: "example.com" },
            subject: "Feedback Request & Link",
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
        label: [Text("Send Feedback")],
    }) |> render_inline()
    actual == expected
}
expect "<a href=\"../one%20more/two.html\">The Link</a>"
    ==Link({
        url: Relative(["..", "one more", "two.html"]),
        label: [Text("The Link")],
    }) |> render_inline()
expect "<a href=\"/about.html\">About</a>"
    ==Link({
        url: Relative(["", "about.html"]),
        label: [Text("About")],
    }) |> render_inline()
expect {
    expected = "<a href=\"javascript%3Aalert%281%29\">The Link</a>"
    actual = Link({
        url: Relative(["javascript:alert(1)"]),
        label: [Text("The Link")],
    }) |> render_inline()
    actual == expected
}

expect "sub.example.co.uk" == Host.from_str("sub.example.co.uk")?.to_str()
expect "xn--mnchen-3ya.de" == Host.from_str("xn--mnchen-3ya.de")?.to_str()
expect "example.com" == Host.from_str("example.com")?.to_str()
expect "example.com" == Host.from_str("Example.Com")?.to_str()
expect "a--b" == Host.from_str("a--b")?.to_str()
expect "123.123.123.123" == Host.from_str("123.123.123.123")?.to_str()
expect {
    host = "x".repeat(63)
        |> List.repeat(3)
        .append("x".repeat(61))
        |> Str.join_with(".")
    host == Host.from_str(host)?.to_str()
}
expect {
    host = "x".repeat(63)
        |> List.repeat(3)
        .append("x".repeat(62))
        |> Str.join_with(".")
    Err(InvalidHost) == Host.from_str(host)
}
expect {
    host = "sub.${"x".repeat(63)}"
    host == Host.from_str(host)?.to_str()
}
expect {
    host = "sub.${"x".repeat(64)}"
    Err(InvalidHost) == Host.from_str(host)
}
expect {
    host = "sub.${"x".repeat(63)}.com"
    host == Host.from_str(host)?.to_str()
}
expect {
    host = "sub.${"x".repeat(64)}.com"
    Err(InvalidHost) == Host.from_str(host)
}
expect Err(InvalidHost) == Host.from_str("")
expect Err(InvalidHost) == Host.from_str("a..b")
expect Err(InvalidHost) == Host.from_str(".com")
expect Err(InvalidHost) == Host.from_str("example.com-")
expect Err(InvalidHost) == Host.from_str("example.com.")
expect Err(InvalidHost) == Host.from_str("example-.com")
expect Err(InvalidHost) == Host.from_str("example.-com")
expect Err(InvalidHost) == Host.from_str("exa\"mple.com")
expect Err(InvalidHost) == Host.from_str("exa mple.com")
expect Err(InvalidHost) == Host.from_str("exa/mple.com")
expect Err(InvalidHost) == Host.from_str("exa:mple.com")
expect Err(InvalidHost) == Host.from_str("exaémple.com")
