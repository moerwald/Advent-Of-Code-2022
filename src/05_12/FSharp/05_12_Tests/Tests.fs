module Tests

open System
open Xunit



(*

    - Get stack index line from file and save it's index
    - Parse the line and find the number of stacks
    - Parse lines before index line
        - from line0 to lineBeforeIndexLine
            - get index for opening bracket [
            - get content of all opening brackets and store it in
              the array corresponding the stack index

        - walk through all stack arrays and reverse the array

    -parse move commands line by line
        -
*)


module AoC =
    let readLines fileName = System.IO.File.ReadAllLines fileName

    let rec getNumberOfStacks index = function
        | h: string :: _ when h.Contains("1") ->
            let arr =
                h.Trim().ToCharArray()
                |> Array.filter (fun x -> x <> ' ')

            let numberOfStacks =
                int (arr[ arr.Length - 1 ].ToString())

            (index, numberOfStacks)
        | _ :: t -> getNumberOfStacks (index + 1) t
        | [] -> (0,0)

    let getCratesFromLine line =
        let allignToFourCharacterBoundary idx = idx + 3
        let calculateArrayIndex idx = idx / 4

        line
        |> Array.ofSeq
        |> Array.indexed
        |> Array.filter (fun x -> Char.IsLetter(snd x))
        |> Array.map (fun x ->
            let arrayIdx =
                fst x
                |> allignToFourCharacterBoundary
                |> calculateArrayIndex

            (arrayIdx, snd x))

    let fillStackArray (lines: string list) (array: string[,]) =
        lines
        |> List.rev
        |> List.map getCratesFromLine
        |> List.iteri (fun i x ->
            x |> Array.iter (fun y ->
                let rowIdx = i
                let columneIdx = (fst y) - 1
                array[rowIdx, columneIdx] <- string (snd y))
            )
        
    let constructCrateArray lines =
       let rows, columns =
           lines |> getNumberOfStacks 0

       let array =
            Array2D.init rows columns (fun _ _ -> " ")

       fillStackArray (lines |> List.take rows) array
       array

    let part1 fileName =
        let lines =
            List.ofArray (readLines fileName)
        constructCrateArray lines
        failwith "XXX"

open AoC
open FsUnit

[<Theory>]
[<InlineData("test_input.txt", 3)>]
[<InlineData("input.txt", 9)>]
let ``getNumberOfStacks check number of returned stack-width`` filename expectedResult =
    List.ofSeq (readLines filename)
    |> getNumberOfStacks 0
    |> snd
    |> should equal expectedResult

[<Theory>]
[<InlineData("test_input.txt", 3)>]
[<InlineData("input.txt", 8)>]
let ``getNumberOfStacks check number of returned stack-height`` filename expectedResult =
    List.ofSeq (readLines filename)
    |> getNumberOfStacks 0
    |> fst
    |> should equal expectedResult

[<Fact>]
let ``get crates from line, assert stack indexes`` () =
    "[L] [F] [G]     [C]     [L] [N] [N]"
    |> getCratesFromLine
    |> Seq.map fst
    |> should equal [ 1; 2; 3; 5; 7; 8; 9 ]

[<Fact>]
let ``get crates from line, assert crates`` () =
    "[L] [F] [G]     [C]     [L] [N] [N]"
    |> getCratesFromLine
    |> Seq.map snd
    |> should equal [ 'L'; 'F'; 'G'; 'C'; 'L'; 'N'; 'N' ]

[<Theory>]
[<InlineData("test_input.txt", 3,  3)>]
[<InlineData("input.txt", 8, 9)>]
let ``constructCrateArray, check returned dimensions`` filename expectedRows expectedColumns =
    let a = List.ofSeq (readLines filename)
            |> constructCrateArray
    Array2D.length1 a |> should equal expectedRows
    Array2D.length2 a |> should equal expectedColumns
    
[<Theory>]
[<InlineData("test_input.txt")>]
let ``constructCrateArray, check content`` filename=
    let (a:string[,]) = List.ofSeq (readLines filename) |> constructCrateArray
    
    a[0,0] |> should equal "Z"
    a[0,1] |> should equal "M"
    a[0,2] |> should equal "P"
            
    a[1,0] |> should equal "N"
    a[1,1] |> should equal "C"
    a[1,2] |> should equal " "
    
    a[2,0] |> should equal " "
    a[2,1] |> should equal "D"
    a[2,2] |> should equal " "
