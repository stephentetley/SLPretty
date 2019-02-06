// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Pretty\Pretty.fs"

open SLFormat.Pretty

type CmdOpts = Doc

let standalone : CmdOpts = text "--standalone"

let args1 = hsep [ standalone ]

let test (doc:CmdOpts) = render 1000 doc

let test01 () = test args1


