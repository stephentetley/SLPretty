// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLFormat


module CmdArgs = 

    open System.Text


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
        
        override x.ToString() : string = 
            let sb = StringBuilder ()
            let inline appendString (s:string) = sb.Append(s) |> ignore
            let rec work (cmds:CmdArgs) (cont : Trace -> unit) : unit = 
                match cmds with
                | Empty -> cont TEmpty
                | NoArg(name) -> 
                    appendString name
                    cont TArg
                | ReqArg (name,punct,value) -> 
                    appendString name
                    appendString (punct.ToString())
                    appendString value
                    cont TArg
                | OptArg (name,punct,ovalue) -> 
                    appendString name
                    match ovalue with
                    | Some value -> 
                        appendString (punct.ToString())
                        appendString value
                    | None -> ()
                    cont TArg
                | Cat(x,y) -> 
                    work x (fun v1 ->
                    if v1 = TArg then appendString " "  else ()
                    work y (fun v2 ->  cont v2))

            work x (fun _ -> ())
            sb.ToString()

        static member (+) (x1:CmdArgs,x2:CmdArgs) = Cat(x1,x2)

    let render (args:CmdArgs) : string = args.ToString()

    let empty : CmdArgs = Empty


    let noArg (name : string) : CmdArgs = NoArg(name)

    let reqArgEquals (name:string) (value:string) : CmdArgs = ReqArg(name,Equals,value)
    
    /// Command with a required argument, printed with a space separator.
    let reqArgSpace (name:string) (value:string) : CmdArgs = ReqArg(name,Space,value)

    /// Command with a required argument, printed with an equals sign separator.
    let optArgEquals (name:string) (value:option<string>) : CmdArgs = OptArg(name,Equals,value)

    let optArgSpace (name:string) (value:option<string>) : CmdArgs = OptArg(name,Space,value)

    let cmdArgs (source: CmdArgs list) : CmdArgs = 
        match source with
        | [] -> Empty
        | (x::xs) -> List.fold (fun y1 y2 -> Cat(y1,y2)) x xs


    // ************************************************************************
    // String helpers
    
    let doubleQuote (s:string) : string = "\"" + s + "\""

    // Double quote a string if it contains spaces - use this e.g. for file names.
    let maybeDoubleQuote (s:string) : string = 
        if s.Contains(" ") then doubleQuote s else s 
    
    let keyValueEquals (key:string) (value:string) : string = 
        sprintf "%s=%s" key value

    let keyValueColon (key:string) (value:string) : string = 
        sprintf "%s:%s" key value

    let (===) (key:string) (value:string) = keyValueEquals key value
