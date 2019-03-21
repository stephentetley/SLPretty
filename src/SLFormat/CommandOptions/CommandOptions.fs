// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace SLFormat.CommandOptions

[<AutoOpen>]
module CommandOptions = 

    open System.Text


    // ************************************************************************
    // String helpers
    
    let doubleQuote (s:string) : string = "\"" + s + "\""
    let singleQuote (s:string) : string = "\"" + s + "\""

    /// Double quoted if the string contains spaces.
    let argValue (s:string) :string = 
        if s.Contains(" ") then 
            s.Replace("\"", "\\\"") |> doubleQuote
        else s
        


    // ************************************************************************
    // Command arguments / options

    type CmdOpt = 
        private | Empty 
                | Text of string
                | Cat of CmdOpt * CmdOpt
        
        override x.ToString() : string = 
            let sb = StringBuilder ()
            let inline appendString (s:string) = sb.Append(s) |> ignore
            let rec work (opt:CmdOpt) (cont : unit -> unit) : unit = 
                match opt with
                | Empty -> cont ()
                | Text(s) -> 
                    appendString s; cont()
                | Cat(x,y) -> 
                    work x (fun _ ->
                    work y cont)

            work x (fun _ -> ())
            sb.ToString()
        
        static member empty : CmdOpt = Empty

    /// Render the options to a string, ready to be passed to 
    /// .Net's `System.Diagnostics.ProcessStartInfo`.
    ///
    /// Note the opts should not contain the program to be invoked, 
    /// this is a separate item when invoking 
    /// `System.Diagnostics.ProcessStartInfo`.
    let renderCmdOpts (opts:CmdOpt list) : string = 
        opts |> List.map (fun x -> x.ToString()) |>  String.concat " "



    /// Create an opt.
    /// Note the argument is just the literal string, no leading 
    /// dashes etc. are added.
    let argument (name : string) : CmdOpt = Text(name)
    
    /// Create a _no opt_.
    let noArgument : CmdOpt = Empty

    /// Literal string (no escaping).
    let literal (s:string) : CmdOpt = Text(s)

    /// Literal character (no escaping).
    let character (c:char) : CmdOpt = Text(c.ToString())
    
    /// Concatentate two CmdOpt fragments, with no space 
    /// between them.
    /// Note - this operator should be considered _internal_.
    /// It is only exposed to let "super users" make their
    /// own custom cammand options.
    let ( ^^ ) (a:CmdOpt) (b:CmdOpt) : CmdOpt = 
        match a, b with
        | Empty,d2 -> d2
        | d1,Empty -> d1
        | _,_ -> Cat(a,b)

    /// Concatentate two CmdOpt fragments, with a space 
    /// seperating them.
    /// Note - this operator should be considered _internal_.
    /// It is only exposed to let "super users" make their
    /// own custom cammand options.
    let ( ^+^ ) (a:CmdOpt) (b:CmdOpt) : CmdOpt = 
        let space = character ' '
        match a, b with
        | Empty,d2 -> d2
        | d1,Empty -> d1
        | _,_ -> Cat(a,(Cat(space, b)))


    let concatArgs (cmd:CmdOpt) (args:string list) : CmdOpt = 
        List.fold (fun ac s -> ac ^^ literal s) cmd args

    /// Print equals `=` between command and value
    let ( &= ) (cmd:CmdOpt) (value:string) : CmdOpt = 
        cmd ^^ character '=' ^^ literal value

    /// Print plus `+` between command and value
    let ( &+ ) (cmd:CmdOpt) (value:string) : CmdOpt = 
        cmd ^^ character '+' ^^ literal value

    /// Print dash `-` between command and value
    let ( &- ) (cmd:CmdOpt) (value:string) : CmdOpt = 
        cmd ^^ character '-' ^^ literal value

    /// Print colon `:` between command and value
    ///
    /// This is COLON not PERCENTAGE. 
    /// FSharp prevents the use of colon in operator names.
    let ( &% ) (key:CmdOpt) (value:string) : CmdOpt = 
        key ^^ character ':' ^^ literal (argValue value)

    /// No space or other punctuation between command and value
    let ( &^ ) (cmd:CmdOpt) (value:string) : CmdOpt = cmd ^^ literal value
    
    /// Print space between command and value
    let ( &^^ ) (cmd:CmdOpt) (value:string) : CmdOpt = cmd ^+^ literal value

    /// Group a list of command options together, separate with space.
    /// This is for convenience so we nest lists of commands
    /// inside a list of commands.
    let group (cmds:CmdOpt list) : CmdOpt = 
        List.fold (^+^) CmdOpt.empty cmds



    // ************************************************************************
    // Extensions
    // We have seen this in Pandoc - maybe it is used elsewhere


    /// An enabled extension is prefixed by plus  '+'
    /// A disabled extension is prefixed by minus '-'
    type Extension =
        | Enable of string
        | Disable of string
        override x.ToString() = 
            match x with
            | Enable s -> "+" + s
            | Disable s -> "-" + s

    let (&**) (cmd:CmdOpt) (extensions:Extension list) : CmdOpt =  
        extensions |> List.map (fun x -> x.ToString()) |> concatArgs cmd 

