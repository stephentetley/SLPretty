// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Tree\RoseTree.fs"

open SLFormat.RoseTree

let tree1 : RoseTree<string> = RoseTree("one", [ RoseTree("two", [RoseTree("four", [])]); RoseTree("three", [])])


let test01 () = 
    tree1 |> drawTree |> printfn "%s"


let tree2 : RoseTree<int> = RoseTree(1, [ RoseTree(2, [RoseTree(4, [RoseTree(5,[]); RoseTree(6,[])])]); RoseTree(3, [])])


let test02 () = 
    tree2 |> mapTree(fun i -> i.ToString()) |> drawTree |> printfn "%s"



