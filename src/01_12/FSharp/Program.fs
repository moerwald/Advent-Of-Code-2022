// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let split (pattern: string) (str: string) = str.Split(pattern)
let splitToElv (text: string) =
    text
    |> split "\r\n\r\n"
    |> Array.map (split "\r\n")
    |> Array.map (Array.map int)
    |> Array.map Array.sum
let elvCalories = splitToElv (File.ReadAllText("input.txt"))

elvCalories
    |> Array.sortDescending
    |> Array.max
    |> printfn "%i"
    
elvCalories
    |> Array.sortDescending
    |> Array.take 3
    |> Array.sum
    |> printfn "%i"
