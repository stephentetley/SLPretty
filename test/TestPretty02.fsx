// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Pretty\Pretty.fs"

open SLFormat.Pretty

let output (doc:Doc) : unit = printDoc 80 doc

let sampleTypes = 
    [ ("empty", "Doc")
    ; ("nest", "Int -> Doc -> Doc")
    ; ("linebreak", "Doc") ]

// PPrint manual page 11
let testFill () = 
    let ptype (name,tp) = fill 6 (text name) ^+^ text "::" ^+^ text tp
    let doc1 : Doc = text "let" ^+^ align (vcat (List.map ptype sampleTypes))
    output doc1

// PPrint manual page 11
let testFillBreak () = 
    let ptype (name,tp) = fillBreak 6 (text name) ^+^ text "::" ^+^ text tp
    let doc1 : Doc = text "let" ^+^ align (vcat (List.map ptype sampleTypes))
    output doc1


let vertical01 () =
    output <| text "hello" ^@^ text "world"


let vertical02 () =
    output <| text "hello" ^@@^ text "world"


let vertical01b () =
    output << internalFlatten <| text "hello" ^@^ text "world"


let vertical02b () =
    output << internalFlatten <| text "hello" ^@@^ text "world"

// output:
// hello (0) world (10)
let column01 () =
    output <| column (fun i -> text "hello" ^+^ parens (intDoc i)) ^+^ column (fun i -> text "world" ^+^ parens (intDoc i))

// output:
// hello (0) world (0) (4)
let nesting01 () =
    output <|  nesting (fun i -> text "hello" ^+^ parens (intDoc i)) 
                ^+^ nesting (fun i -> text "world" ^+^ parens (intDoc i))  
                ^+^ nest 4 (nesting (fun i -> parens (intDoc i)))  

