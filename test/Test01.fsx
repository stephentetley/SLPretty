// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load "..\src\SLPretty.fs"

open SLPretty

let test01 () = 
    prettyPrint empty 80


let test02 () = 
    prettyPrint (text "hello") 80

let testNesting01 () : unit = 
    let a : Doc = text "aaa"
    let b : Doc = text "bbb"
    let c : Doc = text "ccc"
    let d : Doc = text "ddd"
    let doc1 : Doc  = a ^@@^ (nest 2 (b ^@@^ c)) ^@@^ d
    printfn "%s" <| prettyPrint doc1 80


let testIndent01 () : unit = 
    let a : Doc = text "aaa"
    let b : Doc = text "bbb"
    let c : Doc = text "ccc"
    let d : Doc = text "ddd"
    let doc1 : Doc  = a ^@@^ (indent 2 (b ^@@^ c)) ^@@^ d
    printfn "%s" <| prettyPrint doc1 80

