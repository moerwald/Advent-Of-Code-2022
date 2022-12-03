// For more information see https://aka.ms/fsharp-console-apps
open AdventOfCode.Input
open AdventOfCode.Driver

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