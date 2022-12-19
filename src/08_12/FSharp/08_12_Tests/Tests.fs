module Tests

open System
open Xunit
open FsUnit.Xunit

let createArrayFrom (input: string array) =
    let a = input
            |> Array.map (fun x -> x |> Array.ofSeq)
            |> Array.map (fun x -> x |> Array.map (string >> int))
    Array2D.init a.Length a.Length (fun i j -> a[i][j] )

let isVisible (treeMap: int[,]) row column =
    let value = treeMap[row, column]

    let visibleSubArray arr actValue =
        arr |> Seq.filter (fun x -> x >= actValue) |> Seq.length = 0

    let visible =
        visibleSubArray treeMap[0 .. row - 1, column] value
        || visibleSubArray treeMap[(row + 1) .., column] value
        || visibleSubArray treeMap[row, .. column - 1] value
        || visibleSubArray treeMap[row, (column + 1) ..] value

    if visible then 1 else 0

let countVisibleTrees treeMap numberOfColumns numberOfRows =
    seq {
        for r = 0 to numberOfRows - 1 do
            for c = 0 to numberOfColumns - 1 do
                yield isVisible treeMap r c
    }

let part1 input =
    let (treeMap: int[,]) = createArrayFrom input
    let columns = Array2D.length1 treeMap
    let rows = Array2D.length2 treeMap

    countVisibleTrees treeMap columns rows |> Seq.sum

[<Fact>]
let ``Part1, test input`` () =
    let input = System.IO.File.ReadAllLines "test_input.txt"
    part1 input |> should equal 21

[<Fact>]
let ``Part1, input`` () =
    let input = System.IO.File.ReadAllLines "input.txt"
    part1 input |> should equal 1818
