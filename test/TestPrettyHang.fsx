// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Pretty\Pretty.fs"

open SLFormat.Pretty

let output (doc:Doc) : unit = printDoc 80 doc

let node (name : string) (kids : Doc list) : Doc = 
    match kids with
    | [] -> brackets (text name)
    | _ ->  hang 4 (brackets  (text name ^!!^ vcat kids))

let tree1 : Doc = 
    node "factx-fsharp"
        [ node "src" []
        ; node "test" []
        ]

/// This should print with nice 'expected' indentation indicating nesting.
let demo01 () = output tree1
    