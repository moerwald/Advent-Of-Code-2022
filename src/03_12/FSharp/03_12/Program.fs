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
        (Set.ofSeq (rucksack[.. halfIndex - 1]), Set.ofSeq (rucksack[halfIndex..]))

    let getDuplicateItemInRucksack rucksack =
        let compartments =
            rucksack |> splitToCompartments

        Set.intersect (fst compartments) (snd compartments)
        |> Set.maxElement

    let sumOfRucksackItems (items: string []) =
        items
        |> Seq.map getDuplicateItemInRucksack
        |> Seq.map Utils.itemToPriority
        |> Seq.sumBy id

module Part2 =

    let findBadge (items: string []) =
        items
        |> Seq.splitInto (items.Length / 3)
        |> Seq.map (fun x ->
            x
            |> Seq.map Set.ofSeq
            |> Set.intersectMany
            |> Set.maxElement
            |> Utils.itemToPriority)

// Part 1

open Utils
open Part1
open Part2

[ "test_input.txt"; "input.txt" ]
|> Seq.map loadFile
|> Seq.map sumOfRucksackItems
|> Seq.iter (fun x -> printfn $"%i{x}")

// Part 2

let addBadgeToSum (source: int seq) = Seq.sum source

let s =
    "input.txt"
    |> loadFile
    |> findBadge
    |> addBadgeToSum

printfn $"%i{s}"

"AB"
    |> Seq.map (fun x ->
        printfn $"first pipeline stage: %c{x}"
        x)
    |> Seq.map (fun x ->
        printfn $"second pipeline stage: %c{x}"
        x)
    |> Seq.iter (fun x -> printfn $"%c{x}")
    
List.ofSeq "AB"
    |> List.map (fun x ->
        printfn $"first pipeline stage: %c{x}"
        x)
    |> List.map (fun x ->
        printfn $"second pipeline stage: %c{x}"
        x)
    |> List.iter (fun x -> printfn $"%c{x}")
 

type Shape =
  // The value here is the radius.
    | Circle of float
  // The value here is the side length.
    | EquilateralTriangle of double
  // The value here is the side length.
    | Square of double
  // The values here are the height and width.
    | Rectangle of double * double

let area  =
    function
    | Circle radius ->  3.14 * (radius**2)
    | Square side -> side * side
    | Rectangle(h, b) -> h * b
    | EquilateralTriangle s -> (sqrt 3.0) / 4.0 * s * s
 
let c = Circle 2.
let areaCircle = area c
 
    

