module Tests

open System

open Xunit
open FsUnit

module AdventOfCode =
    type FileInfo = { name: string; fileSize: int }

    type DirectoryInfo = { name: string }

    type EmptyInfo = { restOfList: string list }

    type FileSystemNode =
        | File of FileInfo
        | Directory of DirectoryInfo * FileSystemNode list
        | EmptyNode of EmptyInfo

    type SizeType =
        | FileType
        | DirectoryType

    type SizeInfo =
        { name: string
          size: int
          sizeType: SizeType }

    let (|CD|LS|DIR|FILE|) (l: string) =
        if l.StartsWith "$ cd " then
            CD l[5..]
        elif l.StartsWith "$ ls" then
            LS
        elif l.StartsWith "dir" then
            DIR l[4..]
        else
            let s = l.Split " "
            FILE(s[0], s[1])

    let generateFileSystemTree (input: string list) =

        let retrieveRestOfInput node =
            node
            |> List.map (fun x ->
                match x with
                | EmptyNode emptyInfo -> Some emptyInfo
                | _ -> None)
            |> List.minBy (fun x ->
                match x with
                | Some n -> n.restOfList.Length
                | _ -> Int32.MaxValue)

        let rec buildUpTree (lines: string list) : FileSystemNode seq =
            seq {
                match lines with
                | [] -> yield EmptyNode { restOfList = [] }
                | head :: restOfList ->
                    match head with
                    | CD ".." -> yield EmptyNode { restOfList = restOfList }
                    | CD cd ->
                        // Get sub-elements
                        let r = List.ofSeq (buildUpTree restOfList)
                        yield Directory({ name = cd }, r)

                        // Check rest of input, build tree for other directories
                        let rl = retrieveRestOfInput r

                        if rl.IsSome && rl.Value.restOfList.Length > 0 then
                            yield! (buildUpTree rl.Value.restOfList)

                    | FILE (size, name) ->
                        yield File { name = name; fileSize = int size }

                        // Keep recursion going
                        yield! (buildUpTree restOfList)
                    | _ -> yield! (buildUpTree restOfList)
            }

        match input with
        | _ :: t -> Directory({ name = "/" }, List.ofSeq (buildUpTree t))
        | _ -> Directory({ name = " " }, []) // settle the compiler

    let private generateDirectorySizes tree =
        let rec calcDirectorySize (tree: FileSystemNode) =
            match tree with
            | File fi ->
                { name = fi.name
                  size = fi.fileSize
                  sizeType = FileType }
            | EmptyNode _ ->
                { name = ""
                  size = 0
                  sizeType = FileType }
            | Directory (directoryInfo, files) ->
                let size = files |> List.map calcDirectorySize |> Seq.sumBy (fun x -> x.size)

                { name = directoryInfo.name
                  size = size
                  sizeType = DirectoryType }

        let rec getAllDirectoryInfos (t: FileSystemNode) =
            seq {
                match t with
                | Directory (d, f) ->
                    yield Directory(d, f)

                    let r =
                        f
                        |> List.filter (fun x ->
                            match x with
                            | Directory _ -> true
                            | _ -> false)
                        |> List.map getAllDirectoryInfos
                        |> List.collect List.ofSeq

                    yield! r
                | _ -> yield Directory({ name = "" }, []) // settle the compiler
            }

        let dirs = List.ofSeq (getAllDirectoryInfos tree)
        dirs |> List.map calcDirectorySize

    let part1 directoriesLessThan input =
        let tree = generateFileSystemTree input
        let dirSizes = generateDirectorySizes tree
        let dirs = dirSizes |> List.filter (fun x -> x.size < directoriesLessThan)
        dirs |> List.sumBy (fun x -> x.size)


open AdventOfCode

[<Theory>]
[<InlineData("test_input.txt", 95437)>]
[<InlineData("input.txt", 1915606)>]
let part1 file expected =
    let input = System.IO.File.ReadLines file
    part1 100000 (List.ofSeq input) |> should equal expected

