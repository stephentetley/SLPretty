// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Pretty\Pretty.fs"

open SLFormat.Pretty


let test01 () = 
    prettyPrint empty 80


let test02 () = 
    prettyPrint (text "hello") 80

/// "ccc" indented 
let testNesting01 () : unit = 
    let a : Doc = text "aaa"
    let b : Doc = text "bbb"
    let c : Doc = text "ccc"
    let d : Doc = text "ddd"
    let doc1 : Doc  = a ^@@^ (nest 2 (b ^@@^ c)) ^@@^ d
    printfn "%s" <| prettyPrint doc1 80

/// "bbb" and "ccc" indented 
let testIndent01 () : unit = 
    let a : Doc = text "aaa"
    let b : Doc = text "bbb"
    let c : Doc = text "ccc"
    let d : Doc = text "ddd"
    let doc1 : Doc  = a ^@@^ (indent 2 (b ^@@^ c)) ^@@^ d
    printfn "%s" <| prettyPrint doc1 80

let test03 () : unit = 
    printfn "%s" <| prettyPrint (text "hello" ^/^ text "world") 7

let test03a () : unit = 
    printfn "%s" <| prettyPrint (text "hello" ^/^ text "world") 12