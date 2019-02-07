// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Pretty\Pretty.fs"

open SLFormat.Pretty

type CmdOpts = Doc

let standalone : CmdOpts = text "--standalone"
let fromLong : CmdOpts = text "--from"
let toLong : CmdOpts = text "--to"
let output : CmdOpts = text "--output"
let metadata : CmdOpts = text "--metadata"
let concat = hsep
let literal (s:string) : CmdOpts = text s

let (&=) (cmd:CmdOpts) (s:string) : CmdOpts = cmd ^^ character '=' ^^ text s
let (&+) (cmd:CmdOpts) (s:string) : CmdOpts = cmd ^^ character '+' ^^ text s
let (&-) (cmd:CmdOpts) (s:string) : CmdOpts = cmd ^^ character '-' ^^ text s
let (&%) (key:CmdOpts) (value:string) : CmdOpts = 
    let qvalue = if value.Contains(" ") then sprintf "\"%s\"" value else value
    key ^^ character ':' ^^ text qvalue

let args1 = concat [ fromLong       &=  "markdown" &+ "auto_identifiers" &- "raw_html"
                   ; toLong         &=  "html"
                   ; metadata       &=  "pagetile" &% "Sample HTML Output"
                   ; standalone
                   ; output         &=  "output/cmdopts_notes.pdf"
                   ; literal            "cmdopts_notes.md"
                   ]

let test (doc:CmdOpts) = render 1000 doc

let test01 () = test args1


