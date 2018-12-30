// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

namespace SLPretty

[<AutoOpen>]
module SLPretty =
    open System
    open System.Text
    
    let hello name =
        printfn "Hello %s" name
    
    type Doc = 
        private 
            | Nil
            | Cat of Doc * Doc
            | Nest of int * Doc
            | Text of string
            | Line of bool
            | Group of Doc
            | Column of (int -> Doc)
            | Nesting of (int -> Doc)   

    type SimpleDoc = 
        private 
            | SEmpty 
            | SText of string * SimpleDoc          
            | SLine of string * SimpleDoc     




    let extendString (s:string) (spaces:int) = 
        s + String.replicate spaces " "

    let flatten (document:Doc) : Doc = 
        let rec work (doc:Doc) : Doc = 
            match doc with
            | Cat(x,y) -> Cat(work x, work y)
            | Nest(_,x) -> work x
            | Line(true) -> Nil
            | Line(false) -> Text(" ")
            | Group(x) -> work x
            | Column(f) -> Column(work << f)
            | Nesting(f) -> Nesting(work << f)
            | _ -> doc
        work document


    let private isTooBig (text:string) (col:int) (width:int) : bool = 
        col + text.Length > width


    exception ErrBacktrack

    let private layout (width:int) (doc:Doc) : SimpleDoc = 
        let rec best (col:int) (docs: list<string * Doc>) (alternate:bool) =
            match docs with
            | [] -> SEmpty
            | (_, Nil) :: rest ->
                best col rest alternate
            | (iz, Cat(x,y)) :: rest -> 
                best col ((iz,x) :: (iz,y) :: rest) alternate
            | (iz, Nest(n,x)) :: rest -> 
                best col ((extendString iz n,x) :: rest) alternate
            | (iz, Line _) :: rest -> 
                SLine(iz, best iz.Length rest alternate)
            | (iz, Group(x)) :: rest ->
                try
                    best col ((iz, flatten x) :: rest) alternate
                with
                | ErrBacktrack -> best col ((iz, x) :: rest) alternate
            | (iz, Text(t)) :: rest ->
                if (width >= 0) && alternate && isTooBig t col width then
                    raise ErrBacktrack
                else
                    SText(t, best (col + t.Length) rest alternate)
            | (iz, Column(f)) :: rest ->
                best col ((iz, f col) :: rest) alternate
            | (iz, Nesting(f)) :: rest ->
                best col ((iz, f iz.Length) :: rest) alternate
        best 0 [("",doc)] false

    let prettyPrint (doc:Doc) (width:int) : string = 
        let sb = StringBuilder ()
        let rec print (sdoc:SimpleDoc) : unit = 
            match sdoc with
            | SEmpty -> ()
            | SText(t,rest) -> 
                ignore <| sb.Append(t)
                print rest
            | SLine(x,rest) -> 
                ignore <| sb.Append('\n')
                ignore <| sb.Append(x)
                print rest

        print (layout width doc)
        sb.ToString()

    


    // ************************************************************************
    // Primitive printers   

    let empty : Doc = Nil
    
    let nest (i:int) (d:Doc) : Doc = Nest (i,d)
    
    let text (s:string) : Doc = Text s 

    let column (f:int -> Doc) : Doc = Column(f)

    let nesting (f:int -> Doc) : Doc = Nesting(f)

    let group (d:Doc) : Doc = Group(d)

    let line : Doc = Line false
    
    let linebreak : Doc = Line true

    let character (ch:char) : Doc = 
        match ch with
        | '\n' -> line 
        | _ -> text <| ch.ToString()


    let softline : Doc = Group line

    let softbreak : Doc = Group linebreak

    
    

    

    let beside (x:Doc) (y:Doc) : Doc = Cat(x,y)

    // Don't try to define (<>) - it is a reserved operator name in F#


    // aka beside
    let (^^) (x:Doc) (y:Doc) = beside x y

    let besideSpace (x:Doc) (y:Doc) : Doc = x ^^ character ' ' ^^ y

    let (^+^) (x:Doc) (y:Doc) : Doc = besideSpace x y

    let (^@^) (x:Doc) (y:Doc) : Doc = x ^^ line ^^ y
    let (^@@^) (x:Doc) (y:Doc) : Doc = x ^^ linebreak ^^ y

    // ************************************************************************
    // Character printers

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

    /// The document @space@ contains a single space, \" \".
    let space : Doc = character ' '

    /// The document @dot@ contains a single dot, \".\".
    let dot : Doc = character '.'

    /// The document @backslash@ contains a back slash, \"\\\".
    let backslash : Doc = character '\\'

    /// The document @equals@ contains an equal sign, \"=\".
    let equals : Doc = character '='


    let spaces (i:int) : Doc = text <| String.replicate i " "

    // ************************************************************************
    // List concatenation 

    let foldDocs (op:Doc -> Doc -> Doc) (docs:Doc list) : Doc = 
        match docs with
        | [] -> empty
        | (x::xs) -> List.fold op x xs

    let punctuate (sep:Doc) (docs:Doc list) : Doc =
        let rec work acc ds =
            match ds with
            | [] -> acc
            | (x :: xs) -> work (Cat(acc, Cat(sep,x))) xs
        match docs with
        | [] -> empty
        | (x :: xs) -> work x xs

    
    let hcat (docs:Doc list) : Doc = foldDocs beside docs

    let hcatSpace (docs:Doc list) : Doc = punctuate space docs

    let vcat (docs:Doc list) : Doc = punctuate line docs

    let vcatSoft (docs:Doc list) : Doc = punctuate softline docs

    let vcatSoftBreak (docs:Doc list) : Doc = punctuate softbreak docs


    let width d f = 
        column (fun k1 -> d ^^ column (fun k2 -> f (k2 - k1)) )

    let align (d:Doc) = 
        column (fun k -> nesting (fun i -> nest (k - i) d))

    let hang (i:int) (d:Doc) : Doc = align (nest i d)

    let indent (i:int) (d:Doc) : Doc = 
        hang i (spaces i ^^ d)

    let fill f d = 
        width d (fun w -> if w >= f then empty else spaces (f - w))

    let fillBreak f d = 
        width d (fun w -> if w > f then nest f linebreak else spaces (f - w))