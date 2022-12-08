module Tests

open System
open Xunit
open FSharp.Scanf

module AoC =
    
    type Move = {
          _amount: int
          _from: int
          _to: int
        }
    
    type CraneType =
        | CrateMover9000
        | CrateMover9001
    
    let readLines fileName = System.IO.File.ReadAllLines fileName

    let rec getNumberOfStacks index = function
        | h:string :: _ when h.Contains("1") ->
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

    let fillStackArray (lines: string list) (array: string[]) =
        lines
        |> List.rev
        |> List.map getCratesFromLine
        |> List.iter (fun x ->
            x |> Array.iter (fun y ->
                let columnIdx = (fst y) - 1
                let crate = string (snd y)
                let stack = array[columnIdx]
                
                array[columnIdx] <- stack + crate )
            )
        
    let constructCrateArray lines =
       let rows, columns = lines |> getNumberOfStacks 0
       let array = Array.init columns (fun _ -> "")
       fillStackArray (lines |> List.take rows) array
       array
       
    let parseMoves lines =
        lines
        |> List.filter (fun (x:string) -> x.Contains "move" )
        |> List.map (fun x ->
            let _, a, _, f, _, t =  sscanf "%s %d %s %d %s %d" x
            // Adapt to array index
            { _amount = a ; _from = f - 1; _to = t - 1  } )
        
    let moveCrate (array:string[]) (craneType:CraneType) (move:Move)  =
        let from = move._from
        let sourceColumn = array[from]
        let numberOfItemsToKeep = sourceColumn.Length - move._amount
        let keepNumberOfItems (column:string) nrOfItems = column.Remove nrOfItems
        
        let firstPartMove = array[from]
                          |> Seq.skip numberOfItemsToKeep
                          |> Seq.take move._amount
        
        let (stackToMove:string) = match craneType with
                                   | CraneType.CrateMover9000 -> firstPartMove |> Seq.rev 
                                   | _ -> firstPartMove
                                   |> String.Concat
        
        array[from] <- keepNumberOfItems sourceColumn numberOfItemsToKeep
        array[move._to]  <- array[move._to] + stackToMove
        
    let moveCrates fileName cranType =
        let lines = List.ofArray (readLines fileName)
        let crateArray = constructCrateArray lines
        let moves = parseMoves lines
        
        moves
        |> List.iter (moveCrate crateArray cranType)
        
        crateArray
        |> Seq.map (fun x -> x.Substring(x.Length - 1))
        |> String.concat ""
        
    let part1 fileName =
        moveCrates fileName CraneType.CrateMover9000
        
    let part2 fileName =
        moveCrates fileName CraneType.CrateMover9001
        
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
[<InlineData("test_input.txt",   3)>]
[<InlineData("input.txt",  9)>]
let ``constructCrateArray, check returned dimensions`` filename expectedColumns =
    let a = List.ofSeq (readLines filename) |> constructCrateArray
    a.Length |> should equal expectedColumns
    
[<Theory>]
[<InlineData("test_input.txt")>]
let ``constructCrateArray, check content`` filename=
    let (a:string[]) = List.ofSeq (readLines filename) |> constructCrateArray
    
    a[0] |> should equal "ZN"
    a[1] |> should equal "MCD"
    a[2] |> should equal "P"
    
[<Theory>]
[<InlineData("move 1 from 2 to 1", 1, 1 ,0)>]
[<InlineData("move 3 from 1 to 3", 3, 0 ,2)>]
[<InlineData("move 2 from 2 to 1", 2, 1, 0)>]
[<InlineData("move 1 from 1 to 2", 1 ,0 ,1)>]
let ``parse move list`` line amountExpected fromExpected toExpected =
    let result = parseMoves [line]
    result.Head._amount |> should equal amountExpected
    result.Head._from |> should equal fromExpected
    result.Head._to |> should equal toExpected
    
[<Theory>]
[<InlineData("test_input.txt", "CMZ")>]
[<InlineData("input.txt", "QPJPLMNNR")>]
let ``part1, check content`` filename expected  =
    part1 filename
    |> should equal expected
    
[<Theory>]
[<InlineData("test_input.txt", "MCD")>]
[<InlineData("input.txt", "BQDNWJPVJ")>]
let ``part2, check content`` filename expected  =
    part2 filename
    |> should equal expected
