module Tests

open AdventOfCode._04_12.Program
open FsUnit
open Xunit

type TestInput =
    { FileName: string
      ExpectedResultPart1: int 
      ExpectedResultPart2: int }

let adventOfCodeInputs: obj[] list =
    [ [| { FileName = "test_input.txt"
           ExpectedResultPart1 = 2
           ExpectedResultPart2 = 4
           } |]
      [| { FileName = "input.txt"
           ExpectedResultPart1 = 494
           ExpectedResultPart2 = 833
           } |]
      ]

[<Theory>]
[<MemberData(nameof adventOfCodeInputs)>]
let ``Part 1`` data =
    let lines = loadFile data.FileName
    let result = lines |> part1
    
    result |> should equal data.ExpectedResultPart1
    
[<Theory>]
[<MemberData(nameof adventOfCodeInputs)>]
let ``Part 2`` data =
    let lines = loadFile data.FileName
    let result = lines |> part2
    
    result |> should equal data.ExpectedResultPart2
