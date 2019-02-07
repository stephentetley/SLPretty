// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\CmdOpts\CmdArgs.fs"

open SLFormat.CmdArgs


let test01 () = 
    render <| empty + noArg "--preserve-tabs" + reqArgEquals "--metadata" ("theme" === "moon")


