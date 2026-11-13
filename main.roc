# SPDX-License-Identifier: UPL-1.0
# Copyright (c) 2026 Simon Redding
    
## Generate static websites from markdwon
##
app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.28.0/2yhxoR7cZPSYbyUzPWULFw8AA6QEcdsgFNRmzu7pZbst.tar.zst",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/5.0.0/GWrbcSuoXstBeasuzyfMuXe99zFZFLgBhoWavjA7rteZ.tar.zst",
    weaver: "https://github.com/lukewilliamboswell/weaver/releases/download/0.10.2/7CdYzjBAJmprmnK45oTpmH6BXt76ZASN2Ck5uZQEoWsK.tar.zst",
}

import cli.Stdout
import cli.Stderr
import cli.OsStr
import cli.Path
import cli.IOErr
import weaver.Cli
import weaver.Opt
import parser.Markdown
import parser.Yaml
import Html
import MdToHtml

CliArgs : {
    output: Str,
    input: Str,
}

main! = |args| {
    match Cli.parse_or_display_message(cli_parser, args, OsStr.to_raw) {
        Ok(parsed_args) => {
            input_path = parsed_args.input |> OsStr.from_str() |> Path.from_os_str()
            output_path = parsed_args.output |> OsStr.from_str() |> Path.from_os_str()
            if relative_to(input_path, output_path).is_ok() {
                _ = Stderr.line!("Error: output cannot be a subdirectory of input.")
                Err(Exit(5))
            } else {
                files = Stream.custom([input_path], Err(Unknown), expand_or_return!)
                index = build_index!(files)
                if index.io_failed.is_empty() {
                    _ = Stdout.line!("Pages: ${index.pages.len().to_str()}, Assets: ${index.assets.len().to_str()}, Skipped: ${index.skipped.len().to_str()}")
                    match copy_assets!(index.assets, input_path, output_path) {
                        Err(e) => {
                            _ = Stderr.line!("Error copying assets: ${Str.inspect(e)}")
                            Err(Exit(6))
                        }
                        _ =>
                            match make_html!(index, input_path, output_path) {
                                Err(e) => {
                                    _ = Stderr.line!("Error: ${Str.inspect(e)}")
                                    Err(Exit(3))
                                }
                                Ok([]) => Ok({})
                                Ok(errors) => {
                                    _ = Stderr.line!("Error: ${Str.inspect(errors)}")
                                    Err(Exit(4))
                                }
                            }
                    }
                }
                else {
                    _ = Stderr.line!("IO failures: ${Str.inspect(index.io_failed)}")
                    Err(Exit(2))
                }
            }
        }
        Err(Help(msg)) => Stdout.line!(msg)
        Err(Version(msg)) => Stdout.line!(msg)
        Err(InvalidUsage(msg)) => {
            _ = Stderr.line!(msg)
            Err(Exit(1))
        }
    }
}

copy_assets! : List(Path), Path, Path => Try({}, _)
copy_assets! = |assets, from, to|{
    assets.fold_try!({},|_, a| {
        target = to.join(relative_to(from, a)?.to_str()?)
        _ = ensure_creatable!(target)?
        a.copy!(target)
    })
}

ensure_creatable! : Path => Try({}, _)
ensure_creatable! = |path|
    path.to_str()?
        .split_on("/")
        .drop_last(1)
        |> Str.join_with("/")
        |> Path.utf8()
        .create_all!()

make_html! : Index, Path, Path => Try(List(_), _)
make_html! = |index, input_path, output_path|
    index.pages.to_list().fold_try!([], |errors, (path, page_type)|
        match make_output_path(path, input_path, output_path) {
            Ok(p) => {
                _ = ensure_creatable!(p)?
                match page_type {
                    Page(frontmatter) => {
                        match p.write_utf8!(
                            path.read_utf8!()?
                            |> Markdown.parse_str()
                            |> MdToHtml.doc(frontmatter.title)
                            .render()) {
                                Ok({}) => Ok(errors)
                                Err(e) => Ok(errors.append(e))
                        }
                    }
                    NoFrontmatter =>
                        match p.write_utf8!(
                            path.read_utf8!()?
                            |> Markdown.parse_str()
                            |> MdToHtml.doc("")
                            .render()) {
                                Ok({}) => Ok(errors)
                                Err(e) => Ok(errors.append(e))
                        }
                    _ => Ok(errors.append(FileTypeError(path)))
                }
            }
            _ => Ok(errors.append(PathError(path)))
        })

make_output_path : Path, Path, Path -> Try(Path, _)
make_output_path = |file, input, output|
    set_ext(output.join(relative_to(input, file)?.to_str()?), "html")

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
                    help: "Root input directory. (default: ./md)",
                    default: Value("md"),
                }),
                output: Opt.str({
                    long: "output",
                    short: "o",
                    help: "Output path. (default: ./dist)",
                    default: Value("./dist"),
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

Index := {
    assets: List(Path) ?? [],
    pages: Dict(Path, [Page(Frontmatter), NoFrontmatter, InvalidYaml(Yaml.Error), MissingRequiredField(Str)]) ?? Dict.empty(),
    skipped: List((Path, Reason)) ?? [],
    io_failed: List((Path, IOErr)) ??[],
}

build_index! : Stream([Page(Path), Asset(Path), Skipped(Path, Reason), Failed(Path, IOErr)]) => Index
build_index! = |files| {
    files.fold!(Index.{}, update_index!)
}

update_index! : Index, [Page(Path), Asset(Path), Skipped(Path, Reason), Failed(Path, _)] => Index
update_index! = |idx, p|
    match p {
        Page(path) =>
            match path.read_utf8!() {
                Ok(s) =>
                    match s |> Markdown.parse_str() |> Markdown.frontmatter() {
                        Ok(fm) =>
                            { ..idx, pages: idx.pages.insert(path, match load_frontmatter(fm) {
                                Ok(c) => Page(c)
                                Err(e) => e
                            }) }
                        Err(Missing) =>
                            { ..idx, pages: idx.pages.insert(path, NoFrontmatter) }
                    }
                Err(PathErr(e, _)) => {
                    { ..idx, io_failed: idx.io_failed.append((path, e)) }
                }
            }
        Asset(path) => { ..idx, assets: idx.assets.append(path) }
        Skipped(path, reason) => { ..idx, skipped: idx.skipped.append((path, reason)) }
        Failed(path, e) => { ..idx, io_failed: idx.io_failed.append((path, e)) }
    }

load_frontmatter : Str -> Try(Frontmatter, _)
load_frontmatter = |s|
    match Yaml.decode(s) {
        Ok(fm) => Ok(fm)
        Err(e) => {
            Err(e)
        }
    }

Frontmatter : {
    title: Str,
    tags: List(Str)
}

asset_exts = ["css", "jpg", "jpeg", "gif", "png", "svg", "woff", "woff2", "ico"]
dot_dirs = [".", "./", "..", "../"]

Reason : [Hidden, SymLink, Other, UnknownExt, BadPath]

classify : Path, [IsFile, IsDir, IsSymLink, IsOther] -> [Page, Skip(Reason), Asset, Descend]
classify = |path, type|
    match type {
        IsSymLink => Skip(SymLink)
        IsOther => Skip(Other)
        IsDir if dot_dirs.contains(path) => Descend
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
                            _ => Skip(UnknownExt)
                        }
                _ =>
                    if type == IsDir
                        match path.to_str() {
                            Ok(parts) =>
                                match parts.split_on("/").drop_if(|s| s == "") {
                                    [.., name] if name.starts_with(".") => Skip(Hidden)
                                    _ => Descend
                                }
                            _ => Skip(BadPath)
                        }
                    else
                        Skip(UnknownExt)
            }
    }

expect Page == classify("some.md", IsFile)
expect Page == classify("some.MD", IsFile)
expect Skip(Hidden) == classify(".gitignore", IsFile)
expect Skip(Hidden) == classify(".some.md", IsFile)
expect Skip(Hidden) == classify(".git", IsDir)
expect Skip(SymLink) == classify("any", IsSymLink)
expect Skip(Other) == classify("any", IsOther)
expect Skip(UnknownExt) == classify("any", IsFile)
expect Skip(UnknownExt) == classify("archive.tar.gz", IsFile)
expect Skip(UnknownExt) == classify("README", IsFile)
expect Descend == classify("git", IsDir)
expect Descend == classify("git.md", IsDir)
expect Descend == classify(".", IsDir)
expect Descend == classify("..", IsDir)
expect Descend == classify("./", IsDir)
expect Descend == classify("../", IsDir)
expect Descend == classify("./test/md/", IsDir)
expect Descend == classify("./test/md", IsDir)
expect Skip(Hidden) == classify("./test/.md/", IsDir)
expect Skip(Hidden) == classify("./test/.md", IsDir)
expect Asset == classify("some.css", IsFile)
expect Asset == classify("some.png", IsFile)
expect Asset == classify("some.gif", IsFile)
expect Asset == classify("some.svg", IsFile)
expect Asset == classify("some.jpg", IsFile)
expect Asset == classify("some.jpeg", IsFile)
expect Asset == classify("some.woff", IsFile)
expect Asset == classify("some.woff2", IsFile)
expect Asset == classify("some.CSS", IsFile)
expect Asset == classify("some.PNG", IsFile)
expect Asset == classify("some.GIF", IsFile)
expect Asset == classify("some.SVG", IsFile)
expect Asset == classify("some.JPG", IsFile)
expect Asset == classify("some.JPEG", IsFile)
expect Asset == classify("some.WOFF", IsFile)
expect Asset == classify("some.WOFF2", IsFile)

relative_to : Path, Path -> Try(Path, _)
relative_to = |base, path|
    match parts(base)? {
        [.. as ps, ""] => ps
        ps => ps
    }.fold_try(parts(path)?, |ps, bp|
        match ps {
            [p, .. as rest] if p == bp => Ok(rest)
            _ => Err(NotAChild)
        })? |> Str.join_with("/") |> Path.utf8() |> Ok

expect Ok("one/1.md") == relative_to("./test/md/", "./test/md/one/1.md")
expect Ok("one/1.md") == relative_to("test/md/", "test/md/one/1.md")
expect Ok("1.md") == relative_to(".", "./1.md")
expect Err(NotAChild) == relative_to("./test/md/", "./test/one/1.md")

parts : Path -> Try(List(Str), _)
parts = |path|
    path.to_str()?.split_on("/") |> Ok #.drop_if(Str.is_empty) |> Ok

expect [".",  "test", "md"] == parts("./test/md")?
expect [".",  "test", "md", "one", "1.md"] == parts("./test/md/one/1.md")?

set_ext : Path, Str -> Try(Path, _)
set_ext = |path, ext|
    match parts(path)? {
        [.. as root, file] =>
            match file.split_on(".") {
                [.. as rest, _ext] =>
                    root.append(rest.append(ext) |> Str.join_with("."))
                        |> Str.join_with("/")
                        |> Path.utf8()
                        |> Ok
                [] => Err(EmptyFilename)
            }
        [] => Err(EmptyPath)
    }

expect "1.html" == set_ext("1.md", "html")?
expect "one/two/three/1.html" == set_ext("one/two/three/1.md", "html")?
expect "one/two/thr.ee/1.html" == set_ext("one/two/thr.ee/1.md", "html")?
expect "/one/two/three/one.1.html" == set_ext("/one/two/three/one.1.md", "html")?
