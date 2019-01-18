// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLFormat.CmdArgs


[<AutoOpen>]
module CmdArgs = 

    open System.Text

    let doubleQuote (s:string) : string = "\"" + s + "\""

    type private Punctuation = 
        | Space 
        | Equals
        override x.ToString() = 
            match x with 
            | Space -> " "
            | Equals -> "="

    type Trace = 
        | TEmpty
        | TArg

    type CmdArgs = 
        private | Empty 
                | NoArg of string
                | ReqArg of string * Punctuation * string
                | OptArg of string * Punctuation * option<string>
                | Cat of CmdArgs * CmdArgs

        static member (+) (x1:CmdArgs,x2:CmdArgs) = Cat(x1,x2)


        member x.Render() : string = 
            let sb = StringBuilder ()
            let rec work (cmds:CmdArgs) (cont : Trace -> unit) : unit = 
                match cmds with
                | Empty -> cont TEmpty
                | NoArg(name) -> 
                    sb.Append(name) |> ignore 
                    cont TArg
                | ReqArg (name,punct,value) -> 
                    sb.Append(name) |> ignore
                    sb.Append(punct.ToString()) |> ignore
                    sb.Append(value) |> ignore
                    cont TArg
                | OptArg (name,punct,ovalue) -> 
                    sb.Append(name) |> ignore
                    match ovalue with
                    | Some value -> 
                        sb.Append(punct.ToString()) |> ignore
                        sb.Append(value) |> ignore
                    | None -> ()
                    cont TArg
                | Cat(x,y) -> 
                    work x (fun v1 ->
                    if v1 = TArg then sb.Append(' ') |> ignore else ()
                    work y (fun v2 -> 
                    cont v2))

            work x (fun _ -> ()) |> ignore
            sb.ToString()

    let empty : CmdArgs = Empty


    let reqArgEquals (name:string) (value:string) : CmdArgs = ReqArg(name,Equals,value)

    let reqArgSpace (name:string) (value:string) : CmdArgs = ReqArg(name,Space,value)

    let optArgEquals (name:string) (value:option<string>) : CmdArgs = OptArg(name,Equals,value)

    let optArgSpace (name:string) (value:option<string>) : CmdArgs = OptArg(name,Space,value)

    let cmdArgs (source: CmdArgs list) : CmdArgs = 
        match source with
        | [] -> Empty
        | (x::xs) -> List.fold (fun y1 y2 -> Cat(y1,y2)) x xs




    