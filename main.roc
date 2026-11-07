# SPDX-License-Identifier: UPL-1.0
# Copyright (c) 2026 Simon Redding
    
## Generate static websites from markdwon
##
app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.26.0/GddgvsGjiNXrHFW6au5i5dUDbTs8FinQdrgE2Gqmw53t.tar.zst",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/3.0.0/6xsRdRqenJrAb4wG7HaST4Xh349vD5sHAJDsKTK6E3dr.tar.zst",
    weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.1/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}

import cli.Stdout
import cli.Stdin
import cli.Stderr
import cli.OsStr
import cli.Path
import cli.IOErr
import weaver.Cli
import weaver.Opt
import weaver.Param
import parser.Markdown
import parser.Yaml
import Html
import MdToHtml

CliArgs : {
    output: Str,
    input: Str,
}

source =
    \\# Shopping
    \\
    \\Buy these *today*:
    \\
    \\- [x] apples
    \\- [ ] pears
    \\
    \\> Quoted
    \\
    \\```roc
    \\main = 1
    \\```
    \\
    \\| Item | Qty |
    \\| :--- | --: |
    \\| pear | 2 |
    \\
    \\***
    \\

main! = |args| {
    match Cli.parse_or_display_message(cli_parser, args, OsStr.to_raw) {
        Ok(parsed_args) => {
            files = Stream.custom([parsed_args.input |> OsStr.from_str() |> Path.from_os_str()], Unknown, expand_or_return!)
            build!(files, parsed_args.output |> OsStr.from_str() |> Path.from_os_str())
        }
        Err(Help(msg)) => Stdout.line!(msg)
        Err(Version(msg)) => Stdout.line!(msg)
        Err(InvalidUsage(msg)) => {
            _ = Stdout.line!(msg)
            Err(Exit(1))
        }
    }
}

is_hidden : Path -> Bool
is_hidden = |path|
    match path.filename() {
        Ok(fn) => fn.display().starts_with(".")
        Err(_) => False
    }

asset_exts = ["css", "jpg", "jpeg", "gif", "png", "svg"]

expand_or_return! : List(Path) => Try(([Page(Path), Asset(Path), Skipped(Path), Failed(Path, IOErr)], List(Path)), [NoMore])
expand_or_return! = |paths|
    match paths {
        [] => Err(NoMore)
        [path, .. as rest] =>
            match path.type!() {
                Err(PathErr(_, e)) => Ok((Failed(path, e), rest))
                Ok(IsFile) if path.ext().map_ok(Path.display).ok_or("").caseless_ascii_equals("md") =>
                    Ok((Page(path), rest))
                Ok(IsFile) if asset_exts.contains(path.ext().map_ok(Path.display).ok_or("").with_ascii_lowercased()) =>
                    Ok((Asset(path), rest))
                Ok(IsDir) if is_hidden(path) => Ok((Skipped(path), rest))
                Ok(IsDir) =>
                    match path.list!() {
                        Ok(ps) => rest.concat(ps) |> expand_or_return!()
                        Err(PathErr(_, e)) => Ok((Failed(path, e), rest))
                    }
                Ok(_) => Ok((Skipped(path), rest))
            }
    }

cli_parser : Cli.CliParser(CliArgs)
cli_parser =
    Cli.assert_valid(
        Cli.finish(
            {
                input: Opt.str({
                    long: "input",
                    short: "i",
                    help: "Root input directory, - to read files from stdin. (default: md)",
                    default: Value("md"),
                }),
                output: Opt.str({
                    long: "output",
                    short: "o",
                    help: "Output path. (default: dist)",
                    default: Value("dist"),
                }),
            }.Cli,
            {
                name: "fleet-street",
                version: "0.0.1",
                authors: ["Simon Redding <s1m0n.r3dd1ng@gmail.com>"],
                description: "Static site generator. Markdown -> HTML",
                text_style: Color,
            },
        ),
    )

Index : Dict(Path, [
    Page(Frontmatter), Asset, Skipped, NoFrontmatter, IOFailed(IOErr),
    BadFrontmatter([InvalidYaml(Yaml.Error), MissingRequiredField(Str)]),
])

build! : Stream([Page(Path), Asset(Path), Skipped(Path), Failed(Path, IOErr)]), Path => Try({}, _)
build! = |files, _output| {
    index = files.fold!(Dict.empty(), update_index!)
    dbg index
    Ok({})
}

update_index! : Index, [Page(Path), Asset(Path), Skipped(Path), Failed(Path, _)] => Index
update_index! = |idx, p|
    match p {
        Page(path) =>
            match path.read_utf8!() {
                Ok(s) =>
                    match s|> Markdown.parse_str() |> Markdown.frontmatter() {
                        Ok(fm) =>
                            idx.insert(path, match load_frontmatter(fm) {
                                Ok(c) => Page(c)
                                Err(e) => BadFrontmatter(e)
                            })
                        Err(Missing) =>
                            idx.insert(path, NoFrontmatter)
                    }
                Err(PathErr(e, _)) => {
                    idx.insert(path, IOFailed(e))
                }
            }
        Asset(path) => idx.insert(path, Asset)
        Skipped(_) => idx
        Failed(path, e) => idx.insert(path, IOFailed(e))
    }

load_frontmatter : Str -> Try(Frontmatter, _)
load_frontmatter = |s|
    match Yaml.decode(s) {
        Ok(fm) => Ok(fm)
        Err(e) => {
            dbg e
            Err(e)
        }
    }

Frontmatter : {
    title: Str,
    tags: List(Str)
}
