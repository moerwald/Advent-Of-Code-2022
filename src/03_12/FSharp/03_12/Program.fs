open System.IO
open System

let loadFile filePath = File.ReadAllLines filePath

let itemToPriority (item: char) =
    match item with
    | item when Char.IsLower(item) -> int item - 96 // subtract the ASCII offset
    | _ -> int item - 38 // subtract the ASCII offset

let splitToCompartments (rucksack: string) =
    let halfIndex = rucksack.Length / 2
    (Set.ofSeq (rucksack[.. halfIndex - 1]), Set.ofSeq (rucksack[halfIndex  ..]))

let getDuplicateItemInRucksack rucksack =
    let compartments = rucksack |> splitToCompartments
    Set.intersect (fst compartments) (snd compartments) |> Set.maxElement

let sumOfRucksackItems (items: string []) =
    items
    |> Seq.map (fun (x: string) -> getDuplicateItemInRucksack x)
    |> Seq.map itemToPriority
    |> Seq.sumBy id
    
let findBadge (items: string[]) =
    items
    |> Seq.map (fun (x:string) -> Set.ofSeq x)
    |> Set.intersectMany
    |> Set.maxElement
    |> itemToPriority

// Part 1
        
["test_input.txt"; "input.txt"]
|> Seq.map loadFile
|> Seq.map sumOfRucksackItems
|> Seq.iter (fun x -> printfn $"%i{x}")

// Part 2
let load = loadFile "input.txt"
let nrOfLines = load.Length 
let s = load
        |> Seq.splitInto (nrOfLines / 3)
        |> Seq.map findBadge
        |> Seq.sum
        
printfn $"%i{s}"