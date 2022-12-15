module Tests

open Xunit
open FsUnit

module AdventOfCode =
    let rec findDistinctBlock windowSize index (data: string) =
        let markerEnd = index + windowSize
        let max = data[index..markerEnd] |> Seq.countBy id |> Seq.maxBy snd

        match (snd max) with
        | m when m = 1 -> markerEnd + 1
        | _ -> findDistinctBlock windowSize (index + 1) data

    let part1 input = findDistinctBlock 3 0 input

    let part2 input = findDistinctBlock 13 0 input

[<Theory>]
[<Trait("Category", "Part1")>]
[<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7)>]
[<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 5)>]
[<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 6)>]
[<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10)>]
[<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11)>]
let ``Part 1, test input`` data expected =
    AdventOfCode.part1 data |> should equal expected

[<Fact>]
[<Trait("Category", "Part1")>]
let `` Part 1, real data`` () =
    let input = System.IO.File.ReadAllText "input.txt"
    AdventOfCode.part1 input |> should equal 1582

[<Theory>]
[<Trait("Category", "Part2")>]
[<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19)>]
[<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", 23)>]
[<InlineData("nppdvjthqldpwncqszvftbrmjlhg", 23)>]
[<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29)>]
[<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26)>]
let ``Part 2, test input`` data expected =
    AdventOfCode.part2 data |> should equal expected

[<Fact>]
[<Trait("Category", "Part2")>]
let `` Part 2, real data`` () =
    let input = System.IO.File.ReadAllText "input.txt"
    AdventOfCode.part2 input |> should equal 3588
