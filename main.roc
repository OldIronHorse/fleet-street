# SPDX-License-Identifier: UPL-1.0
# Copyright (c) 2026 Simon Redding

## Generate static websites from markdwon
##
app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.26.0/GddgvsGjiNXrHFW6au5i5dUDbTs8FinQdrgE2Gqmw53t.tar.zst",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/3.0.0/6xsRdRqenJrAb4wG7HaST4Xh349vD5sHAJDsKTK6E3dr.tar.zst",
}

import cli.Stdout
import parser.Markdown
import Html

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

main! = |_args| {
    blocks = Markdown.parse_str(source)
    for block in blocks {
        Stdout.line!(Str.inspect(block))?
    }
    loose_list!()
}

loose_list! = || {
    blocks = Markdown.parse_str("3. first\n\n4. second\n")
    for block in blocks {
        Stdout.line!(Str.inspect(block))?
    }
    Ok({})
}
