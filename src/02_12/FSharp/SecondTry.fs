module AdventOfCode.SecondTry

type Selection =
    | Rock
    | Paper
    | Scissors

type Round = Selection * Selection

type Result =
    | Win
    | Draw
    | Loose

let toSelection selection =
    match selection with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> failwith "Undefined input"


let toResult round = 
    match round with
    | Scissors, Rock -> Win
    | Rock, Paper -> Win
    | Paper, Scissors -> Win
    | p1, p2 -> if p1 = p2 then Draw else Loose
    
let toScore (result:Result) =
        match result with
        | Win -> 6
        | Draw -> 3
        | Loose -> 0

let playRound (round: Round) =
    let selectionScore =
        match (snd round) with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3
        
    (round |> toResult |> toScore) + selectionScore

let resultToPint result =
    match result with
    | Win -> 6
    | Draw -> 3
    | _ -> 0

let toRounds (input:string) =
    let split = input.Split(" ")
    (toSelection split[0], toSelection split[1])

let lines =
    System.IO.File.ReadLines("input.txt")
let calcPlayScore =
    lines
    |> Seq.map toRounds 
    |> Seq.map playRound
    |> Seq.sum
    
