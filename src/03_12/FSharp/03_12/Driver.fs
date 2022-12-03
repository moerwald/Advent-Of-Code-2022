module AdventOfCode.Driver

open System
open System.Text

let itemToPriority (item: char) =
    let value =
        int (Encoding.ASCII.GetBytes(string item)[0])
        
    printfn "item : %c" item

    match item with
    | i when Char.IsLower(i) -> value - 96 // subtract the ASSCI offset
    | _ -> value - 38 // subtract the ASSCI offset

let countCharacters (lst: char list) = lst |> Seq.groupBy id

let getCharacterIncludingPrio lst =
    lst
    |> Seq.map (fun x ->
        let _char = fst x

        {| Item = _char
           Occurence = Seq.length (snd x)
           Priority = itemToPriority _char |})

let splitToCompartments (rucksack: string) =
    let halfIndex = (rucksack.Length / 2)

    let fstCompartment =
        rucksack.Substring(0, halfIndex)

    let sndCompartment =
        rucksack.Substring(halfIndex, halfIndex)

    (Set.ofSeq fstCompartment, Set.ofSeq sndCompartment)

let getDuplicateItemInRucksack (rucksack: string) =
    let compartments =
        rucksack |> splitToCompartments

    let intersection =
        Set.intersect (fst compartments) (snd compartments)

    Set.maxElement intersection

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
    
    
