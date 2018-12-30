// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

#load "..\src\SLPretty.fs"

open SLPretty

let test01 () = 
    prettyPrint empty 80


let test02 () = 
    prettyPrint (text "hello") 80
