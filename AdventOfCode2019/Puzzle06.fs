module Puzzle06
open Microsoft.FSharp.Collections


type OrbitDefinition = {
    name: string;
    center: string;
}

let parseDescription (description: string) =
    let parts = description.Split(')');
    { name = parts.[1]; center = parts.[0] }

type OrbitTree = Map<string, OrbitDefinition>

let buildTree (descriptions: seq<OrbitDefinition>) : OrbitTree =
    let d =
        descriptions
        |> Seq.map (fun desc -> (desc.name, desc))

    new Map<string, OrbitDefinition> (d)

let buildFromFile filename : OrbitTree =
    System.IO.File.ReadAllLines(filename)
    |> Seq.map parseDescription
    |> buildTree

let depth (tree: OrbitTree) item =
    let rec depth' item count =
        let object = tree.TryFind item
        match object with 
            | None -> count
            | Some obj -> depth' obj.center (count + 1)

    depth' item 0

let depthTo (tree: OrbitTree) root item =
    let rec recurse item count =
        let object = tree.[item]
        if object.center = root then
            count + 1
        else
            match tree.TryFind(object.center) with
                | None -> failwithf "Never found parent %A" root
                | Some parent -> recurse parent.name (count + 1)

    recurse item 0


let totalDepth tree =
    let depthCounter = depth tree
    tree
    |> Seq.map (fun def -> def.Key)
    |> Seq.map depthCounter
    |> Seq.sum

let ancestors (tree: OrbitTree) node =
    let rec ancestors' l n =
        match tree.TryFind(n.center) with
            | None -> (n.center :: l)
            | Some parent -> ancestors' (n.center :: l) parent

    ancestors' [] tree.[node]

let rec commonAncestor (a1: string list) (a2: string list) =
    let rec recurse t1 t2 current =
        let h1 = List.head t1
        let h2 = List.head t2
        if h1 <> h2 then
            current
        else
            recurse (List.tail t1) (List.tail t2) h1

    recurse a1 a2 (List.head a1)
        


let transferDistance tree a b =
    let ancestors' = ancestors tree
    let commonPoint = commonAncestor (ancestors' a) (ancestors' b)
    let depth' = depthTo tree commonPoint

    (depth' a) + (depth' b) - 2 // subtract 2 since we are going parent to parent

