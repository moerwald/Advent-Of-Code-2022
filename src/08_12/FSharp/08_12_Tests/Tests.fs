module Tests

open System
open Xunit
open FsUnit.Xunit

let createArrayFrom (input: string array) =
    let a =
        input
        |> Array.map (fun x -> x |> Array.ofSeq)
        |> Array.map (fun x -> x |> Array.map (string >> int))

    Array2D.init a.Length a.Length (fun i j -> a[i][j])


let getDirections (treeMap: int[,]) row column =

    [| treeMap[0 .. row - 1, column] |> Array.rev // UP
       treeMap[(row + 1) .., column]
       treeMap[row, .. column - 1] |> Array.rev // LFT
       treeMap[row, (column + 1) ..] |] // RIGHT

let isVisible (treeMap: int[,]) row column =

    let visibleSubArray arr actValue =
        arr |> Seq.filter (fun x -> x >= actValue) |> Seq.length = 0

    let value = treeMap[row, column]

    let visible =
        getDirections treeMap row column
        |> Array.map (fun x -> visibleSubArray x value)
        |> Array.exists (fun x -> x = true)

    if visible then 1 else 0

let calcScore (treeMap: int[,]) row column =

    let calcTreeScore (arr: int[]) actValue =
        if arr.Length < 1 then
            0
        else if (arr |> Array.max < actValue) then
            arr.Length
        else
            (arr |> Array.takeWhile (fun x -> x < actValue) |> Array.length) + 1

    let score =
        getDirections treeMap row column
        |> Array.map (fun x -> calcTreeScore x (treeMap[row, column]))
        |> Array.fold (fun acc x -> acc * x) 1

    {| Row = row
       Column = column
       Score = score |}


let walkTroughTreeMap treeMap numberOfColumns numberOfRows func =
    seq {
        for r = 0 to numberOfRows - 1 do
            for c = 0 to numberOfColumns - 1 do
                yield func treeMap r c
    }

let part1 input =
    let (treeMap: int[,]) = createArrayFrom input
    let columns = Array2D.length1 treeMap
    let rows = Array2D.length2 treeMap

    walkTroughTreeMap treeMap columns rows isVisible |> Seq.sum


let part2 input =
    let (treeMap: int[,]) = createArrayFrom input
    let columns = Array2D.length1 treeMap
    let rows = Array2D.length2 treeMap

    let scores = walkTroughTreeMap treeMap columns rows calcScore
    scores |> Seq.maxBy (fun x -> x.Score)

[<Fact>]
let ``Part1, test input`` () =
    let input = System.IO.File.ReadAllLines "test_input.txt"
    part1 input |> should equal 21

[<Fact>]
let ``Part1, input`` () =
    let input = System.IO.File.ReadAllLines "input.txt"
    part1 input |> should equal 1818

[<Fact>]
let ``Part2, test input`` () =
    let input = System.IO.File.ReadAllLines "test_input.txt"
    let s = part2 input
    Console.WriteLine s
    s.Score |> should equal 8

[<Fact>]

let ``Part2, input`` () =
    let input = System.IO.File.ReadAllLines "input.txt"
    let s = part2 input
    Console.WriteLine s
    s.Score |> should equal 8
