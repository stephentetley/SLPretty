// Copyright (c) Stephen Tetley 2018,2019
// License: BSD 3 Clause

// Acknowledgment
// ==============
// This is an implementation of Daan Leijen's PPrint. 
// The original Haskell library was strictified for Mercury by 
// Ralph Becket and subsequently ported to Racket by David 
// Herman.
// The CPS transformation of layout and other functions in this
// implementation is new. 
// Any mistakes are mine (SPT).


namespace SLFormat


module Pretty =
    
    open System
    open System.Text
    
    /// The abstract type of Documents.
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

    /// The type representing a rendered Doc.
    type private SimpleDoc = 
        | SEmpty 
        | SText of string * SimpleDoc          
        | SLine of string * SimpleDoc     




    let private padRight (s:string) (spaces:int) = s.PadRight(totalWidth=spaces, paddingChar=' ')


    let private flatten (document:Doc) : Doc = 
        let rec work (doc:Doc) (cont : Doc -> Doc) : Doc = 
            match doc with
            | Cat(x,y) -> 
                work x (fun x1 -> work y (fun y1 -> cont (Cat(x1, y1))))
            | Nest(_,x) -> work x cont
            | Line(true) -> cont Nil
            | Line(false) -> cont (Text(" "))
            | Group(x) -> work x cont
            | Column(f) -> 
                cont (Column(fun i -> work (f i) id))           // Check!
            | Nesting(f) -> 
                cont (Nesting(fun i -> work (f i) id))          // Check!
            | _ -> cont doc
        work document (fun x -> x)


    let private isTooBig (text:string) (col:int) (width:int) : bool = 
        col + text.Length > width



    let private layout (width:int) (doc:Doc) : SimpleDoc = 
        let rec best (col:int) (docs: list<string * Doc>) (alternate:bool) sk fk =
            match docs with
            | [] -> sk SEmpty
            | (_, Nil) :: rest ->
                best col rest alternate sk fk
            | (iz, Cat(x,y)) :: rest -> 
                best col ((iz,x) :: (iz,y) :: rest) alternate sk fk
            | (iz, Nest(n,x)) :: rest -> 
                best col ((padRight iz n,x) :: rest) alternate sk fk
            | (iz, Line _) :: rest ->
                best iz.Length rest alternate (fun v1 -> sk (SLine(iz,v1))) fk
            | (iz, Group(x)) :: rest ->
                best col ((iz, flatten x) :: rest) true (fun v1 -> sk v1) (fun _ -> 
                best col ((iz, x) :: rest) alternate sk fk)    
            | (iz, Text(t)) :: rest ->
                if (width >= 0) && alternate && isTooBig t col width then
                    fk ()
                else
                    best (col + t.Length) rest alternate (fun v1 -> sk (SText(t,v1))) fk
            | (iz, Column(f)) :: rest ->
                best col ((iz, f col) :: rest) alternate sk fk
            | (iz, Nesting(f)) :: rest ->
                best col ((iz, f iz.Length) :: rest) alternate sk fk
        best 0 [("",doc)] false id (fun () -> SEmpty)

    /// Pretty print the document to a string.
    /// Lines are terminated with the operating systems default line terminator.
    let prettyPrint (doc:Doc) (width:int) : string = 
        let sb = StringBuilder ()
        let inline stringAppend (s:string) :unit = sb.Append(s) |> ignore
        let rec work (sdoc:SimpleDoc) (cont:unit -> unit) : unit = 
            match sdoc with
            | SEmpty -> cont ()
            | SText(t,rest) -> 
                stringAppend t
                work rest cont
            | SLine(x,rest) -> 
                sb.AppendLine() |> ignore
                stringAppend x
                work rest cont

        work (layout width doc) (fun _ -> ())
        sb.ToString()

    /// Render the document to a string.
    /// This is `prettyPrint` with arg order reversed
    let render (width:int) (doc:Doc) : string = prettyPrint doc width

    /// Output a document to file.
    /// Lines are terminated with the default line terminator.
    let writeDoc (width:int) (fileName:string) (doc:Doc) : unit = 
        use sw = IO.File.CreateText(fileName)
        let rec work (sdoc:SimpleDoc) (cont:unit -> unit) : unit = 
            match sdoc with
            | SEmpty -> cont ()
            | SText(t,rest) -> 
                sw.Write(t)     |> ignore
                work rest cont
            | SLine(x,rest) -> 
                sw.WriteLine()  |> ignore
                sw.Write(x)     |> ignore 
                work rest cont

        work (layout width doc) (fun _ -> ())

    


    // ************************************************************************
    // Primitive printers   


    /// The empty document
    let empty : Doc = Nil
    
    /// 'nest' renders the document 'doc' with the current indentation level 
    /// increased by i
    let nest (i:int) (doc:Doc) : Doc = Nest (i,doc)
    
    /// Generate the document containing the literal string 's'.
    /// The input text should not contain newline characters.
    let text (s:string) : Doc = Text s

    /// Undocumented - internal?
    let column (f:int -> Doc) : Doc = Column(f)

    /// Undocumented - internal?
    let nesting (f:int -> Doc) : Doc = Nesting(f)

    /// Use the group combinator to specify alternate layouts.
    /// `(group doc)` undoes all linebreaks in doc.
    let group (doc:Doc) : Doc = Group(doc)

    /// 'line' advances to the next line and indents to the current nesting 
    /// level.
    /// If the line break is undone by group line is rendered as a space.
    let line : Doc = Line false

    /// 'linebreak' advances to the next line and indents to the current nesting 
    /// level.
    /// If the line break is undone by group line is rendered as empty.    
    let linebreak : Doc = Line true

    /// This is 'char' in PPrint (Haskell).
    let character (ch:char) : Doc = 
        match ch with
        | '\n' | '\r' -> line 
        | _ -> text <| ch.ToString()

    /// `softline` behaves like `space` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    let softline : Doc = group line

    /// `softbreak` behaves like `empty` if the document it is part of fits the page.
    /// If it is too large it renders as `line`.
    let softbreak : Doc = group linebreak

    
    

    
    /// Concatenate documents x and y.
    let beside (x:Doc) (y:Doc) : Doc = Cat(x,y)

    // Don't try to define (<>) - it is a reserved operator name in F#


    /// Concatenate two documents horizontally (no separating space).
    /// This is (<>) in PPrint (Haskell).
    let (^^) (x:Doc) (y:Doc) = beside x y

    /// Concatenate two documents horizontally with a separating space.
    let besideSpace (x:Doc) (y:Doc) : Doc = x ^^ character ' ' ^^ y


    /// Concatenate two documents horizontally with a separating space.
    /// This is (<+>) in PPrint (Haskell).
    let (^+^) (x:Doc) (y:Doc) : Doc = besideSpace x y

    /// Concatenate two documents with a soft line.
    /// This is (</>) in PPrint (Haskell).
    let (^/^) (x:Doc) (y:Doc) : Doc = x ^^ softline ^^ y
    
    /// Concatenate two documents with a soft break.
    /// This is (<//>) in PPrint (Haskell).
    let (^//^) (x:Doc) (y:Doc) : Doc = x ^^ softbreak ^^ y

    /// Concatenate two documents separinting with `line`.
    /// This is (<$>) in PPrint (Haskell).
    let (^@^) (x:Doc) (y:Doc) : Doc = x ^^ line ^^ y

    /// Concatenate two documents separinting with `linebreak`.
    /// This is (<$$>) in PPrint (Haskell).
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

    /// Generate a document of n spaces.
    let spaces (n:int) : Doc = text <| String.replicate n " "

    /// Enclose the document body betwen l (left) and r (right).
    let enclose (l:Doc) (r:Doc) (body:Doc)   = l ^^ body ^^ r

    /// Enclose in signle quotes '...'
    let squotes (x:Doc) : Doc = enclose squote squote x
    
    /// Enclose in double quotes "..."
    let dquotes (x:Doc) : Doc = enclose dquote dquote x
    
    /// Enclose in angle braces {...}
    let braces (x:Doc) : Doc = enclose lbrace rbrace x
    
    /// Enclose in square brackets (...)
    let parens (x:Doc) : Doc = enclose lparen rparen x
    
    /// Enclose in angle brackets <...>
    let angles (x:Doc) : Doc = enclose langle rangle x
    
    /// Enclose in square brackets [...]
    let brackets (x:Doc) : Doc = enclose lbracket rbracket x

    // ************************************************************************
    // List concatenation 

    let foldDocs (op:Doc -> Doc -> Doc) (documents:Doc list) : Doc = 
        let rec work (acc:Doc) (ls:Doc list) (cont:Doc -> Doc) : Doc = 
            match ls with
            | [] -> cont acc
            | x :: xs -> work (op acc x) xs cont        
        match documents with
        | [] -> empty
        | (x::xs) -> work x xs id

    let punctuate (separator:Doc) (documents:Doc list) : Doc =
        foldDocs (fun l r -> l ^^ separator ^^ r) documents
    
    /// Separate documents horizontally with a space.
    let hsep (documents: Doc list) = foldDocs (^+^) documents

    /// Separate documents with (^@^)
    let vsep (documents: Doc list) = foldDocs (^@^) documents
    
    /// Separate documents with (^/^)
    let fillSep (documents: Doc list)  = foldDocs (^/^) documents

    let sep (documents: Doc list) = group (vsep documents)

    let hcat (documents: Doc list) = foldDocs (^^) documents

    /// Separate documents with (^@@^)
    let vcat (documents: Doc list) = foldDocs (^@@^) documents

    /// Separate documents with (^//^)
    let fillCat (documents: Doc list)  = foldDocs (^//^) documents

    let cat (documents: Doc list) = group (vcat documents)

    /// Concatenante all documents with `separator` and bookend them 
    /// with `left` and `right`.
    let encloseSep (left:Doc) (right:Doc) (separator:Doc) (documents:Doc list) : Doc = 
        let rec work (acc:Doc) (docs:Doc list) (cont:Doc -> Doc) = 
            match docs with
            | [] -> cont acc
            | [x] -> cont (acc ^//^ x)
            | x :: xs -> 
                work (acc ^//^ x ^^ separator) xs cont
        work left documents (fun d -> d ^^ right)

    /// Enclose in square brackets and separate with comma [a,b,c,...]
    let commaList (docs:Doc list) : Doc = encloseSep lbracket rbracket comma docs

    /// Enclose in square brackets and separate with semicolon [a;b;c,...]
    let semiList (docs:Doc list) : Doc = encloseSep lbracket rbracket semi docs

    /// Enclose in parens and separate with comma (a,b,c,...)
    let tupled (docs:Doc list) : Doc = encloseSep lparen rparen comma docs

    /// Enclose in curly braces and separate with comma {a,b,c,...}
    let commaBraces  (docs:Doc list) : Doc = encloseSep lbrace rbrace comma docs

    /// Enclose in curly braces and separate with semicolon {a;b;c;...}
    let semiBraces  (docs:Doc list) : Doc = encloseSep lbrace rbrace semi docs

    let hcatSpace (docs:Doc list) : Doc = punctuate space docs


    let vcatSoft (docs:Doc list) : Doc = punctuate softline docs

    let vcatSoftBreak (docs:Doc list) : Doc = punctuate softbreak docs

    /// Undocumented.
    let width (doc:Doc) (fn:int -> Doc) : Doc = 
        column (fun k1 -> doc ^^ column (fun k2 -> fn (k2 - k1)) )

    /// `(align d)` renders the document `d` with the nesting level set to 
    /// the current column.
    let align (doc:Doc) :Doc = 
        column (fun k -> nesting (fun i -> nest (k - i) doc))

    /// Implement hanging indentation.
    let hang (i:int) (doc:Doc) : Doc = align (nest i doc)

    /// Indent the document `doc` with `i` spaces.
    let indent (i:int) (doc:Doc) : Doc = 
        hang i (spaces i ^^ doc)

    /// `fill` renders the supplied  document and right pads with spaces 
    /// until the width is equal to `i`.
    let fill (i:int) (doc:Doc) : Doc = 
        width doc (fun w -> if w >= i then empty else spaces (i - w))

    let fillBreak (f:int) (doc:Doc) : Doc = 
        width doc (fun w -> if w > f then nest f linebreak else spaces (f - w))

    /// Use this rather than text if the input string contains newlines.
    /// Newline characters are replaced by 'line'
    /// This is 'string' in PPrint (Haskell).
    let fromString (s:string) : Doc = 
        let lines = List.map text << Array.toList <| s.Split([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
        punctuate line lines


