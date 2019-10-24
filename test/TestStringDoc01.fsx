// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\StringDoc\StringDoc.fs"

open SLFormat.StringDoc


let empty01 () = 
    render empty 


let text01 () = 
    text "hello"
        |> render


let beside01 () = 
    text "hello" ^^ text "world"
        |> render


let besideSpace01 () = 
    text "hello" ^+^ text "world"
        |> render

let below01 () = 
    text "hello" ^!^ text "world"
        |> render

let belowSpace01 () = 
    text "hello" ^!!^ text "world"
        |> render

