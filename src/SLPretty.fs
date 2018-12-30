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
        | Nil
        | Cat of Doc * Doc
        | Nest of int * Doc
        | Label of string * Doc
        | Markup of unit * Doc        // TODO
        | Text of string
        | Line of bool
        | Group of Doc
        | Column of (int -> Doc)
        | Nesting of (int -> Doc)   

    type SimpleDoc = 
        | SEmpty 
        | SText of string * SimpleDoc
        | SPush of unit * SimpleDoc      // TODO
        | SPop of SimpleDoc              // TODO
        | SLine of string * SimpleDoc      // TODO



    let line : Doc = Line false

    // break is reserved
    let docbreak : Doc = Line true

    let softLine : Doc = Group line

    let softBreak : Doc = Group docbreak

    let empty : Doc = Nil

    let nest (i:int) (d:Doc) : Doc = Nest (i,d)

    let text (s:string) : Doc = Text s 

    let character (ch:char) : Doc = 
        match ch with
        | '\n' -> line 
        | _ -> text <| ch.ToString()


    let extendString (s:string) (spaces:int) = 
        s + String.replicate spaces " "

    let flatten (document:Doc) : Doc = 
        let rec work (doc:Doc) : Doc = 
            match doc with
            | Cat(x,y) -> Cat(work x, work y)
            | Nest(_,x) -> work x
            | Label(_,x) -> work x
            | Markup(f,x) -> Markup(f, work x)
            | Line(true) -> Nil
            | Line(false) -> Text(" ")
            | Group(x) -> work x
            | Column(f) -> Column(work << f)
            | Nesting(f) -> Nesting(work << f)
            | _ -> doc
        work document


    let isTooBig (text:string) (col:int) (width:int) : bool = 
        col + text.Length > width

    type private LayoutAns = 
        | String of String
        | False

    exception ErrBacktrack

    let layout (width:int) (doc:Doc) : SimpleDoc = 
        let rec best (col:int) (docs: list<LayoutAns * Doc>) (alternate:bool) =
            match docs with
            | [] -> SEmpty
            | (False, _) :: rest -> 
                SPop(best col rest alternate)
            | (_, Nil) :: rest ->
                best col rest alternate
            | (iz, Cat(x,y)) :: rest -> 
                best col ((iz,x) :: (iz,y) :: rest) alternate
            | (String iz, Nest(n,x)) :: rest -> 
                best col ((String <| extendString iz n,x) :: rest) alternate
            | (String iz, Label(l,x)) :: rest -> 
                best col ((String <| iz + l, x) :: rest) alternate
            | (iz, Markup(f,x)) :: rest -> 
                SPush(f, best col ((iz, x) :: (False, Nil) :: rest) alternate) 
            | (String iz, Line _) :: rest -> 
                SLine(iz, best iz.Length rest alternate)
            | (iz,Text(t)) :: rest ->
                if (width >= 0) && alternate && isTooBig t col width then
                    raise ErrBacktrack
                else
                    SText(t, best (col + t.Length) rest alternate)
            | (iz,Column(f)) :: rest ->
                best col ((iz, f col) :: rest) alternate
            | (String iz,Nesting(f)) :: rest ->
                best col ((String iz, f iz.Length) :: rest) alternate
        best 0 [(String "",doc)] false

    let prettyPrint (doc:Doc) (width:int) : string = 
        let sb = StringBuilder ()
        let rec print (sdoc:SimpleDoc) : unit = 
            match sdoc with
            | SEmpty -> ()
            | SText(t,rest) -> 
                ignore <| sb.Append(t)
                print rest
            | SPush(_, rest) -> 
                print rest
            | SPop(rest) ->
                print rest
            | SLine(x,rest) -> 
                ignore <| sb.Append('\n')
                ignore <| sb.Append(x)
                print rest

        print (layout width doc)
        sb.ToString()