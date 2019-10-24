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
    let beside (x : Doc) (y : Doc) : 
        Doc = x >> y


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

    let punctuate (sep : Doc) (docs : Doc list) : Doc = 
        match docs with
        | [] -> id
        | d1 :: rest -> 
            fun sb -> List.fold (fun ac fn -> ac |> sep |> fn) (d1 sb) rest

    let hcat (docs : Doc list) : Doc = 
        punctuate empty docs

    let hsep (docs : Doc list) : Doc = 
        punctuate space docs

    let vcat (docs : Doc list) : Doc = 
        punctuate newline docs

    let vsep (docs : Doc list) : Doc = 
        punctuate (newline >> newline) docs

    // ************************************************************************
    // Character printers
    
    /// Tab
    let tab : Doc = 
        fun sb -> sb.Append '\t'


    /// Single left parenthesis: '('
    let lparen : Doc = character '('

    /// Single right parenthesis: ')'
    let rparen : Doc = character ')'

    /// Single left angle: '<'
    let langle : Doc = character '<'

    /// Single right angle: '>'
    let rangle : Doc = character '>'

    /// Single left brace: '{'
    let lbrace : Doc = character '{'

    /// Single right brace: '}'
    let rbrace : Doc= character '}'

    /// Single left square bracket: '['
    let lbracket : Doc = character '['

    /// Single right square bracket: ']'
    let rbracket : Doc = character ']'


    /// Single quote: '
    let squote : Doc= character '\''

    ///The document @dquote@ contains a double quote, '\"'.
    let dquote : Doc = character '"'

    /// The document @semi@ contains a semi colon, \";\".
    let semi : Doc = character ';'

    /// The document @colon@ contains a colon, \":\".
    let colon : Doc = character ':'

    /// The document @comma@ contains a comma, \",\".
    let comma : Doc = character ','

    /// The document @dot@ contains a single dot, \".\".
    let dot : Doc = character '.'

    /// The document @backslash@ contains a back slash, \"\\\".
    let backslash : Doc = character '\\'

    /// The document @equals@ contains an equal sign, \"=\".
    let equals : Doc = character '='


    // ************************************************************************
    // Further combinators

    /// Generate a document of n spaces.
    let spaces (n : int) : Doc = 
        text <| String.replicate n " "

    /// Enclose the document body betwen l (left) and r (right).
    let enclose (left : Doc) (right : Doc) (body : Doc)   = 
        left ^^ body ^^ right

    /// Enclose in signle quotes '...'
    let squotes (doc : Doc) : Doc = enclose squote squote doc
    
    /// Enclose in double quotes "..."
    let dquotes (doc : Doc) : Doc = enclose dquote dquote doc
    
    /// Enclose in angle braces {...}
    let braces (doc : Doc) : Doc = enclose lbrace rbrace doc
    
    /// Enclose in square brackets (...)
    let parens (doc : Doc) : Doc = enclose lparen rparen doc
    
    /// Enclose in angle brackets <...>
    let angles (doc : Doc) : Doc = enclose langle rangle doc
    
    /// Enclose in square brackets [...]
    let brackets (doc : Doc) : Doc = enclose lbracket rbracket doc


    /// Concatenante all documents with `separator` and bookend them 
    /// with `left` and `right`.
    let encloseSep (left : Doc) (right : Doc) (separator : Doc) (documents : Doc list) : Doc = 
        let rec work (acc : Doc) (docs : Doc list) (cont : Doc -> Doc) = 
            match docs with
            | [] -> cont acc
            | [x] -> cont (acc ^^ x)
            | x :: xs -> 
                work (acc ^^ x ^^ separator) xs cont
        work left documents (fun d -> d ^^ right)

    /// Enclose in square brackets and separate with comma [a,b,c,...]
    let commaList (docs : Doc list) : Doc = 
        encloseSep lbracket rbracket comma docs

    /// Enclose in square brackets and separate with semicolon [a;b;c,...]
    let semiList (docs : Doc list) : Doc = 
        encloseSep lbracket rbracket semi docs

    /// Enclose in parens and separate with comma (a,b,c,...)
    let tupled (docs : Doc list) : Doc = 
        encloseSep lparen rparen comma docs

    /// Enclose in curly braces and separate with comma {a,b,c,...}
    let commaBraces (docs : Doc list) : Doc = 
        encloseSep lbrace rbrace comma docs

    /// Enclose in curly braces and separate with semicolon {a;b;c;...}
    let semiBraces (docs : Doc list) : Doc = 
        encloseSep lbrace rbrace semi docs

    /// Print an int literal.
    let intDoc (i:int) : Doc = i.ToString() |> text
    
    /// Print a float literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let floatDoc (d:float) : Doc = d.ToString() |> text
    
    /// Print a double literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let doubleDoc (d:double) : Doc = d.ToString() |> text

    /// Print a single (float32) literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let singleDoc (d:single) : Doc = d.ToString() |> text

    /// Print a decimal literal.
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let decimalDoc (d:decimal) : Doc = d.ToString() |> text

    /// Prints "true" or "false" (lowercase, F# style)
    let boolDoc (b : bool) : Doc = 
        fun sb -> sb.Append(b)



    /// Print a unsigned byte literal as a decimal.
    /// Note no F# type specifying suffix is printed, if you want this
    /// functionality you need to write your own function.
    let byteDoc (i : byte) : Doc = 
        fun sb -> sb.Append(i)
        
    /// Print a signed byte literal as a decimal.
    let sbyteDoc (i : sbyte) : Doc = 
        fun sb -> sb.Append(i)

    /// Print a 16-bit signed byte literal as a decimal.
    let int16Doc (i : int16) : Doc = 
        fun sb -> sb.Append(i)

    /// Print a 16-bit unsigned byte literal as a decimal.
    let uint16Doc (i : uint16) : Doc = 
        fun sb -> sb.Append(i)

    /// Print a 32-bit signed byte literal as a decimal.
    let int32Doc (i : int32) : Doc = 
        fun sb -> sb.Append(i)

    /// Print a 32-bit unsigned byte literal as a decimal.
    let uint32Doc (i : uint32) : Doc = 
        fun sb -> sb.Append(i)

    /// Print a 64-bit signed byte literal as a decimal.        
    let int64Doc (i : int64) : Doc = 
        fun sb -> sb.Append(i)

    /// Print a 64-bit unsigned byte literal as a decimal.
    let uint64Doc (i : uint64) : Doc = 
        fun sb -> sb.Append(i)
    
    /// Print a 32-bit IEEE float. 
    /// The output uses ToString() so it may be printed in 
    /// exponential notation.
    let float32Doc (d : float32) : Doc = 
        fun sb -> sb.Append(d)

    