Url := [
    Http(HttpSpec),
    MailTo(MailToSpec),
    Relative(RelativeSpec),
].{
    is_eq : Url, Url -> Bool
    is_eq = |this, that|
        match (this, that) {
            (Http(a), Http(b)) => a == b
            (MailTo(a), MailTo(b)) => a == b
            (Relative(a), Relative(b)) => a == b
            _ => False
        }

    to_str : Url -> Str
    to_str = |url|
        match url {
            Http(hs) => {
                port = hs.port.map_ok(|p| ":${p.to_str()}").ok_or("")
                "${hs.scheme.to_str()}://${hs.host.to_str()}${port}/${path(hs.path)}${query(hs.query)}${fragment(hs.fragment)}"
            }
            MailTo(mt) => {
                subject = if mt.subject.is_empty() [] else ["subject=${url_encode(mt.subject)}"]
                body = if mt.body.is_empty() [] else ["body=${url_encode(mt.body)}"]
                cc = if mt.cc.is_empty() [] else ["cc=${mt.cc.map(Address.url_encode) |> Str.join_with(",")}"]
                bcc = if mt.bcc.is_empty() []  else ["bcc=${mt.bcc.map(Address.url_encode) |> Str.join_with(",")}"]
                args = [subject, body, cc, bcc]
                    .join()
                    |> Str.join_with("&")
                "mailto:${mt.to.url_encode()}${if args.is_empty() "" else args.with_prefix("?")}"
            }
            Relative(rs) => {
                root = if rs.root "/" else ""
                "${root}${path(rs.path)}${query(rs.query)}${fragment(rs.fragment)}"
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
    }.{
        is_eq = |this, that|
            this.host == that.host
                and this.port == that.port
                and this.path == that.path
                and this.scheme == that.scheme
                and this.query == that.query
                and this.fragment ==  that.fragment
    }

    Scheme := [Http, Https].{
        to_str : Scheme -> Str
        to_str = |s|
            match s {
                Http => "http"
                Https => "https"
            }

        is_eq = |this, that|
            match (this, that) {
                (Http, Http) | (Https, Https) => True
                _ => False
            }
    }

    QueryParam := { key: Str, value: Str }.{
        url_encode : QueryParam -> Str
        url_encode = |qp|
            "${url_encode(qp.key)}=${url_encode(qp.value)}"

        is_eq = |this, that|
            this.key == that.key
                and this.value == that.value
    }

    Address := { recipient: Str, domain: Host }.{
        url_encode : Address -> Str
        url_encode = |addr|
            "${url_encode(addr.recipient)}@${url_encode(addr.domain.to_str())}"

        is_eq = |this, that|
            this.recipient == that.recipient
                and this.domain == that.domain
    }

    MailToSpec := {
        to: Address,
        subject: Str ?? "",
        body: Str ?? "",
        cc: List(Address) ?? [],
        bcc: List(Address)?? [],
    }.{
        is_eq = |this, that|
            this.to == that.to
                and this.subject == that.subject
                and this.body == that.body
                and this.cc == that.cc
                and this.bcc == that.bcc
    }

    RelativeSpec := {
        root: Bool ?? False,
        path: List(Str) ?? [],
        fragment: Try(Str, [None]) ?? Err(None),
        query: List(QueryParam) ?? [],
    }.{
        is_eq = |this, that|
            this.root == that.root
                and this.path == that.path
                and this.fragment == that.fragment
                and this.query == that.query
    }
}

path = |ps|
    ps |> drop_while(Str.is_empty)
    .map(url_encode)
    |> Str.join_with("/")

query = |qs|
    if qs.is_empty()
        ""
    else
        qs.map(|p| p.url_encode())
            |> Str.join_with("&")
            .with_prefix("?")

fragment = |fg|
    fg.map_ok(|f| "#${url_encode(f)}").ok_or("")

drop_while : List(a), (a -> Bool) -> List(a)
drop_while = |l, f|
    match l {
        [h, .. as rest] if f(h) => rest |> drop_while(f)
        _ => l
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

hex_digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"]
hex_digit = |n| hex_digits.get(n.to_u64()).ok_or("0")

as_hex : U8 -> Str
as_hex = |n| "${n.div_by(16) |> hex_digit()}${n.rem_by(16) |> hex_digit()}"

expect "00" == as_hex(0)
expect "0A" == as_hex(10)
expect "0F" == as_hex(15)
expect "10" == as_hex(16)
expect "FF" == as_hex(255)

expect "http://example.com/one/two/three/four.html"
    == Http({ scheme: Http, host: Host.from_str("example.com")?, path: ["one", "two", "three", "four.html"] })
        |> Url.to_str()

expect "https://example.com/"
    == Http({ host: Host.from_str("example.com")? })
        |> Url.to_str()

expect "https://example.com/one/two/three/four.html"
    == Http({ host: Host.from_str("example.com")?, path: ["one", "two", "three", "four.html"] })
        |> Url.to_str()

expect "https://example.com/one/two//three/four.html"
    == Http({ host: Host.from_str("example.com")?, path: ["one", "two", "", "three", "four.html"] })
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

expect "https://example.com:0/one/two/three/four.html"
    == Http({
        host: Host.from_str("example.com")?,
        port: Ok(0),
        path: ["one", "two", "three", "four.html"]
    }) |> Url.to_str()

expect "https://example.com:65535/one/two/three/four.html"
    == Http({
        host: Host.from_str("example.com")?,
        port: Ok(65535),
        path: ["one", "two", "three", "four.html"]
    }) |> Url.to_str()

expect "https://example.com/one/two/three/four.html?name=Joe%20Bl%26ggs&age=4%235"
    == Http({
        host: Host.from_str("example.com")?,
        path: ["one", "two", "three", "four.html"],
        query: [{ key: "name", value: "Joe Bl&ggs" }, { key: "age", value: "4#5" }],
    }) |> Url.to_str()

expect "https://example.com/one/two/three/four.html?name=Joe%20Bl%26ggs&age=4%235#intro"
    == Http({
        host: Host.from_str("example.com")?,
        path: ["one", "two", "three", "four.html"],
        query: [{ key: "name", value: "Joe Bl&ggs" }, { key: "age", value: "4#5" }],
        fragment: Ok("intro"),
    }) |> Url.to_str()

expect "https://example.com/one/two/three/four.html"
    == Http({
        host: Host.from_str("example.com")?,
        path: ["one", "two", "three", "four.html"],
        query: [],
    }) |> Url.to_str()

expect "https://example.com/one/two/three/"
    == Http({
        host: Host.from_str("example.com")?,
        path: ["one", "two", "three", ""],
        query: [],
    }) |> Url.to_str()

expect "https://example.com/#intro"
    == Http({
        host: Host.from_str("example.com")?,
        fragment: Ok("intro")
    }) |> Url.to_str()

expect Http({ host: Host.from_str("example.com") })
    != Http({ host: Host.from_str("google.com") })
expect Http({ host: Host.from_str("example.com") })
    == Http({ host: Host.from_str("Example.Com") })

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
expect Err(InvalidHost) == Host.from_str("-")
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

expect "%0A" == url_encode("\n")

expect "about.html" == Relative({ path: ["", "", "about.html"] }) |> Url.to_str()
expect "about/" == Relative({ path: ["", "", "about", ""] }) |> Url.to_str()
expect "#intro" == Relative({ fragment: Ok("intro") }) |> Url.to_str()
expect {
    expected = "/about.html"
    actual = Relative({ root: True, path: ["about.html"] }) |> Url.to_str()
    actual == expected
}
expect "/" == Relative({ root: True }) |> Url.to_str()
expect "" == Relative({}) |> Url.to_str()
expect "?name=Joe%20Bl%26ggs&age=4%235"
    == Relative({
        query: [{ key: "name", value: "Joe Bl&ggs" }, { key: "age", value: "4#5" }],
    }) |> Url.to_str()
expect "/?name=Joe%20Bl%26ggs&age=4%235#intro"
    == Relative({
        root: True,
        fragment: Ok("intro"),
        query: [{ key: "name", value: "Joe Bl&ggs" }, { key: "age", value: "4#5" }],
    }) |> Url.to_str()

expect {
    expected = "mailto:someone@example.com?subject=Feedback%20Request%20%26%20Link"
    actual = MailTo({
        to: { recipient: "someone", domain: Url.Host.from_str("example.com")? },
        subject: "Feedback Request & Link",
    }) |> Url.to_str()
    actual == expected
}

expect {
    expected ="mailto:someone@example.com?subject=Feedback%20Request%20%26%20Link&body=Please%20provide%20feedback.&cc=cc1@elsewhere.com,cc2@example.com&bcc=bc1@elsewhere.com,bc2@example.com"
    actual = MailTo({
        to: { recipient: "someone", domain: Url.Host.from_str("example.com")? },
        subject: "Feedback Request & Link",
        body: "Please provide feedback.",
        cc: [
            { recipient: "cc1", domain: Url.Host.from_str("elsewhere.com")? },
            { recipient: "cc2", domain: Url.Host.from_str("example.com")? },
        ],
        bcc: [
            { recipient: "bc1", domain: Url.Host.from_str("elsewhere.com")? },
            { recipient: "bc2", domain: Url.Host.from_str("example.com")? },
        ],
    }) |> Url.to_str()
    actual == expected
}
