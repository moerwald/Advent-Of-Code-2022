open System.IO
open System

module Utils =
    let itemToPriority (item: char) =
        match item with
        | item when Char.IsLower(item) -> int item - 96 // subtract the ASCII offset
        | _ -> int item - 38 // subtract the ASCII offset
        
    let loadFile filePath = File.ReadAllLines filePath

module Part1 =
    let splitToCompartments (rucksack: string) =
        let halfIndex = rucksack.Length / 2
        (Set.ofSeq (rucksack[.. halfIndex - 1]), Set.ofSeq (rucksack[halfIndex  ..]))

    let getDuplicateItemInRucksack rucksack =
        let compartments = rucksack |> splitToCompartments
        Set.intersect (fst compartments) (snd compartments) |> Set.maxElement

    let sumOfRucksackItems (items: string []) =
        items
        |> Seq.map getDuplicateItemInRucksack
        |> Seq.map Utils.itemToPriority
        |> Seq.sumBy id
    
module Part2 =
    let findBadge (items: string[]) =
        items
        |> Seq.map Set.ofSeq 
        |> Set.intersectMany
        |> Set.maxElement
        |> Utils.itemToPriority

// Part 1

open Utils
open Part1
open Part2
        
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