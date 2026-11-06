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

expand_or_return! : List(Path) => Try((Try(Path, _), List(Path)), _)
expand_or_return! = |paths|
    match paths {
        [] => Err(NoMore)
        [path, .. as rest] =>
            match path.type!() {
                Err(e) => Ok((Err(e), rest))
                Ok(IsFile) if path.ext().ok_or("").caseless_ascii_equals("md") => Ok((Ok(path), rest))
                Ok(IsDir) if is_hidden(path) => Ok((Err(Hidden(path)), rest))
                Ok(IsDir) =>
                    match path.list!() {
                        Ok(ps) => rest.concat(ps) |> expand_or_return!()
                        Err(e) => Ok((Err(e), rest))
                    }
                Ok(_) => Ok((Err(NotMarkdown(path)), rest))
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

Index : Dict(Path, Try(Frontmatter, [Hidden, Missing, NotMarkdown, PathErr(IOErr,Path)]))

build! : Stream(Try(Path, [Hidden(Path), NotMarkdown(Path), PathErr(IOErr, Path)])), Path => Try({}, _)
build! = |files, _output| {
    index = files.fold!(Dict.empty(), update_index!)
    dbg index
    Ok({})
}

update_index! : Index, Try(Path, [Hidden(Path), NotMarkdown(Path), PathErr(IOErr, Path)]) => Index
update_index! = |idx, p|
    match p {
        Ok(path) =>
            match path.read_utf8!() {
                Ok(s) =>
                    match s|> Markdown.parse_str() |> Markdown.frontmatter() {
                        Ok(fm) =>
                            idx.insert(path, Ok(load_frontmatter(fm).ok_or({ title: "", tags: []}))) # parse frontmatter, add to index
                        Err(Missing) =>
                            idx.insert(path, Err(Missing))
                    }
                Err(PathErr(e, path_)) => {
                    idx.insert(path, Err(PathErr(e, path_)))
                }
            }
        Err(Hidden(path)) => idx.insert(path, Err(Hidden))
        Err(NotMarkdown(path)) => idx.insert(path, Err(NotMarkdown))
        Err(PathErr(e, path)) => idx.insert(path, Err(PathErr(e, path)))
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
