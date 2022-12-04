module Tests

open AdventOfCode._04_12.Program
open FsUnit
open Xunit

type TestInput =
    { FileName: string
      ExpectedResult: int }

let adventOfCodeInputs: obj[] list =
    [ [| { FileName = "test_input.txt"
           ExpectedResult = 2 } |]
      [| { FileName = "input.txt";ExpectedResult = 494 } |]
      ]

[<Theory>]
[<MemberData(nameof adventOfCodeInputs)>]
let ``Part 1 TestA`` data =
    let lines = loadFile data.FileName
    let result = lines |> part1
    
    result |> should equal data.ExpectedResult
