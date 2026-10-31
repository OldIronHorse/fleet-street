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

expect "%0A" == url_encode("\n")
