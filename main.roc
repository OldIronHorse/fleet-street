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
            _ = Stderr.line!(msg)
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

expand_or_return! : List(Path) => Try(([Page(Path), Asset(Path), Skipped(Path, Reason), Failed(Path, IOErr)], List(Path)), [NoMore])
expand_or_return! = |paths|
    match paths {
        [] => Err(NoMore)
        [path, .. as rest] =>
            match path.type!() {
                Err(PathErr(e, _)) => Ok((Failed(path, e), rest))
                Ok(type) =>
                    match classify(path, type) {
                        Page => Ok((Page(path), rest))
                        Asset => Ok((Asset(path), rest))
                        Skip(reason) => Ok((Skipped(path, reason), rest))
                        Descend =>
                            match path.list!() {
                                Ok(ps) => rest.concat(ps) |> expand_or_return!()
                                Err(PathErr(e, _)) => Ok((Failed(path, e), rest))
                            }
                    }
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

build! : Stream([Page(Path), Asset(Path), Skipped(Path, Reason), Failed(Path, IOErr)]), Path => Try({}, _)
build! = |files, _output| {
    index = files.fold!(Dict.empty(), update_index!)
    dbg index
    Ok({})
}

update_index! : Index, [Page(Path), Asset(Path), Skipped(Path, Reason), Failed(Path, _)] => Index
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
        Skipped(_, _) => idx
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

asset_exts = ["css", "jpg", "jpeg", "gif", "png", "svg", "woff", "woff2"]

Reason : [Hidden, SymLink, Other, Unknown]

classify : Path, [IsFile, IsDir, IsSymLink, IsOther] -> [Page, Skip(Reason), Asset, Descend]
classify = |path, type|
    match type {
        IsSymLink => Skip(SymLink)
        IsOther => Skip(Other)
        IsFile | IsDir =>
            match path.filename() {
                Ok(filename) if filename.display().starts_with(".") => Skip(Hidden)
                Ok(_) =>
                    if type == IsDir
                        Descend
                    else
                        match path.ext() {
                            Ok(md) if md.display().caseless_ascii_equals("md") => Page
                            Ok(ext) if asset_exts.contains(ext.display().with_ascii_lowercased()) => Asset
                            _ => Skip(Unknown)
                        }
                _ => Skip(Unknown)
            }
    }

expect Page == classify("some.md", IsFile)
expect Page == classify("some.MD", IsFile)
expect Skip(Hidden) == classify(".gitignore", IsFile)
expect Skip(Hidden) == classify(".some.md", IsFile)
expect Skip(Hidden) == classify(".git", IsDir)
expect Skip(SymLink) == classify("any", IsSymLink)
expect Skip(Other) == classify("any", IsOther)
expect Skip(Unknown) == classify("any", IsFile)
expect Descend == classify("git", IsDir)
expect Asset == classify("some.css", IsFile)
expect Asset == classify("some.png", IsFile)
expect Asset == classify("some.svg", IsFile)
expect Asset == classify("some.jpg", IsFile)
expect Asset == classify("some.jpeg", IsFile)
expect Asset == classify("some.woff", IsFile)
expect Asset == classify("some.woff2", IsFile)
expect Asset == classify("some.CSS", IsFile)
expect Asset == classify("some.PNG", IsFile)
expect Asset == classify("some.SVG", IsFile)
expect Asset == classify("some.JPG", IsFile)
expect Asset == classify("some.JPEG", IsFile)
expect Asset == classify("some.WOFF", IsFile)
expect Asset == classify("some.WOFF2", IsFile)

