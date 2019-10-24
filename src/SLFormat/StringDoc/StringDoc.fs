// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

// Build strings with System.Text.StringBuilder

// Note 
// This library presents a nice "pretty printer" like interface
// because it is a good API, it does not make any effort to actually 
// pretty print the document - there is no "fitting" of the document. 

namespace SLFormat


module StringDoc =
    
    open System
    open System.Text
    
    type Doc = StringBuilder -> StringBuilder

    let render (doc : Doc) : string = 
        new StringBuilder ()
            |> doc
            |> fun sb -> sb.ToString ()

    /// The empty document
    let empty : Doc = id 

    /// Generate the document containing the literal string 's'.
    /// Any newlines will be persisted in the output.
    let text (s:string) : Doc = 
        fun sb -> sb.Append(s)

    /// This is 'char' in PPrint (Haskell).
    let character (ch:char) : Doc = 
        fun sb -> sb.Append(ch)


    let space : Doc = text " "

    let newline : Doc = 
        fun sb -> sb.AppendLine()

    /// Concatenate documents x and y.
    let beside (x : Doc) (y : Doc) : Doc = x >> y

    // Don't try to define (<>) - it is a reserved operator name in F#


    /// Concatenate two documents horizontally (no separating space).
    /// This is (<>) in PPrint (Haskell).
    let ( ^^ ) (x : Doc) (y : Doc) = 
        beside x y

    /// Concatenate two documents horizontally with a separating space.
    let besideSpace (x : Doc) (y : Doc) : Doc = 
        x ^^ character ' ' ^^ y


    /// Concatenate two documents horizontally with a separating space.
    /// This is (<+>) in PPrint (Haskell).
    let ( ^+^ ) (x : Doc) (y : Doc) : Doc = 
        besideSpace x y

    /// Concatenate two documents, print the second on a new line.    
    let ( ^!^ ) (x : Doc) (y : Doc) : Doc = 
        x ^^ newline ^^ y

    /// Concatenate two documents, print a blank line between them.
    let ( ^!!^ ) (x : Doc) (y : Doc) : Doc = 
        x ^^ newline ^^ newline ^^ y




