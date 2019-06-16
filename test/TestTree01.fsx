// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#load "..\src\SLFormat\Tree\Tree.fs"

open SLFormat.Tree

let tree1 : Tree<string> = Tree("one", [ Tree("two", [Tree("four", [])]); Tree("three", [])])


let test01 () = 
    tree1 |> drawTree |> printfn "%s"


let tree2 : Tree<int> = Tree(1, [ Tree(2, [Tree(4, [Tree(5,[]); Tree(6,[])])]); Tree(3, [])])


let test02 () = 
    tree2 |> mapTree(fun i -> i.ToString()) |> drawTree |> printfn "%s"



