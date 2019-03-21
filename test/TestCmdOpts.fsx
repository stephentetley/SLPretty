// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\CommandOptions\CommandOptions.fs"

open SLFormat.CommandOptions


let standalone : CmdOpt     = argument "--standalone"
let fromLong : CmdOpt       = argument "--from"
let toLong : CmdOpt         = argument "--to"
let output : CmdOpt         = argument "--output"
let metadata : CmdOpt       = argument "--metadata"



let args1 () : CmdOpt list =
    [ fromLong       &=  "markdown" &+ "auto_identifiers" &- "raw_html"
    ; toLong         &=  "html"
    ; metadata       &=  "pagetile" &% "Sample HTML Output"
    ; standalone
    ; output         &=  "output/cmdopts_notes.pdf"
    ; literal            "cmdopts_notes.md"
    ]

let test01 () = renderCmdOpts (args1 ()) |> printfn "%s" 
