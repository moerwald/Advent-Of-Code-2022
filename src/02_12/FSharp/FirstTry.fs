module AdventOfCode.FirstTry

    let combinations =
        Map [ ("A X", 4)
              ("A Y", 8)
              ("A Z", 3)

              ("B X", 1)
              ("B Y", 5)
              ("B Z", 9)

              ("C X", 7)
              ("C Y", 2)
              ("C Z", 6) ]

    (*
    A, X Rock
    B, Y Paper
    C, Z Scissors

    X Loose
    Y Draw
    Z Win
    *)
    // Map strategy to win, loose, draw combinations
    let gameStrategy =
        Map [ ("A X", "A Z")
              ("A Y", "A X")
              ("A Z", "A Y")

              ("B X", "B X")
              ("B Y", "B Y")
              ("B Z", "B Z")

              ("C X", "C Y")
              ("C Y", "C Z")
              ("C Z", "C X") ]

    let lines =
        System.IO.File.ReadLines("input.txt")


    let totalWinPoints =
        lines
        |> Seq.map (fun x -> Map.find x combinations)
        |> Seq.sum
    let totalWinPointsBasedOnStrategy =
        lines
        |> Seq.map (fun x -> Map.find x gameStrategy)
        |> Seq.map (fun x -> Map.find x combinations)
        |> Seq.sum
