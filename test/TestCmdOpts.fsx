// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\CmdOpts\CmdOpts.fs"

open SLFormat.CmdOpts


let standalone : CmdOpt     = command "--standalone"
let fromLong : CmdOpt       = command "--from"
let toLong : CmdOpt         = command "--to"
let output : CmdOpt         = command "--output"
let metadata : CmdOpt       = command "--metadata"



let args1 = options [ fromLong       &=  "markdown" &+ "auto_identifiers" &- "raw_html"
                    ; toLong         &=  "html"
                    ; metadata       &=  "pagetile" &% "Sample HTML Output"
                    ; standalone
                    ; output         &=  "output/cmdopts_notes.pdf"
                    ; literal            "cmdopts_notes.md"
                    ]

let test01 () = printfn "%s" args1
