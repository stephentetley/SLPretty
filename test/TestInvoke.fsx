// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
open System.IO

#load "..\src\SLFormat\CommandOptions\CommandOptions.fs"
#load "..\src\SLFormat\CommandOptions\SimpleInvoke.fs"

open SLFormat.CommandOptions
open SLFormat.CommandOptions.SimpleInvoke

#load "CreateDb.fs"
open SLFormat.CommandOptions.CreateDb

let workingDirectory () : string = 
    Path.Combine(__SOURCE_DIRECTORY__, @"..\data")


let temp01 () = 
    let cwd = workingDirectory ()
    let ddl = "create_db.sql"
    createDb cwd "TEMP_DB.sqlite" ddl

//let temp02 () = 
//    let cwd = workingDirectory () |> Some
//    runEchoCmd cwd [literal "hello"] (outputFile "output.txt")


//let temp03 () = 
//    let cwd = workingDirectory () |> Some
//    runPipeToEcho cwd "\"hello world from f#\"" (outputFile "output2.txt")

//let temp04 () = 
//    try
//        let cwd = workingDirectory () |> Some
//        runPipeDir cwd |> Some
//    with
//    | expn -> printfn "%s" expn.Message ; None