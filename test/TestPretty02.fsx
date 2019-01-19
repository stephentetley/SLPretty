// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Pretty\Pretty.fs"

open SLFormat


let beside = Pretty.beside

let text = Pretty.text

let d1 = beside (text "one") (Pretty.character 'c')

let test = Pretty.render 50 d1





