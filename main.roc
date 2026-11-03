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
import cli.OsStr
import cli.Path
import weaver.Cli
import weaver.Opt
import weaver.Param
import parser.Markdown
import Html
import MdToHtml

CliArgs : {
    input: Str,
    output: Str,
    changed: List(Str),
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
    \\<div>raw</div>

main! = |args| {
    match Cli.parse_or_display_message(cli_parser, args, OsStr.to_raw) {
        Ok(parsed_args) => {
            match parsed_args.changed {
                [] => build!(
                    parsed_args.input |> OsStr.from_str() |> Path.from_os_str(),
                    parsed_args.output |> OsStr.from_str() |> Path.from_os_str())
                fs => rebuild!(
                    parsed_args.input |> OsStr.from_str() |> Path.from_os_str(),
                    fs.map(|f| f |> OsStr.from_str() |> Path.from_os_str()),
                    parsed_args.output |> OsStr.from_str() |> Path.from_os_str())
            }
        }
        Err(Help(msg)) => Stdout.line!(msg)
        Err(Version(msg)) => Stdout.line!(msg)
        Err(InvalidUsage(msg)) => Stdout.line!(msg)
    }
}

cli_parser : Cli.CliParser(CliArgs)
cli_parser =
    Cli.assert_valid(
        Cli.finish(
            {
                changed: Opt.str_list({
                    short: "c",
                    long: "changed",
                    help: "Changed files for a partial rebuild.",
                }),
                input: Opt.str({
                    long: "input",
                    short: "i",
                    help: "Input path. (default: .)",
                    default: Value("."),
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

rebuild! : Path, List(Path), Path => Try({}, _)
rebuild! = |_input, _files, _output| Err(NotImplemented)

build! : Path, Path => Try({}, _)
build! = |input, _output| {
    input |> walk_files!()?.for_each!(|f| {
        _ = Stdout.line!(Str.inspect(f))
    })
    Ok({})
}

walk_files! : Path => Try(Stream(Path), _)
walk_files! = |path|
    Stream.custom(
        path.list!()?,
        Unknown,
        expand_dir
    ) |> Ok

expand_dir = |ps|
    match ps {
        []  => Err(NoMore)
        [p, .. as rest] if p.is_dir!().map_err(|_| NoMore)? => expand_dir(rest.concat(p.list!().map_err(|_| NoMore)?))
        [p, .. as rest] => Ok((p, rest))
    }
