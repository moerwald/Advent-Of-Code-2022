module AdventOfCode.Input

open System.IO

let loadFile filePath =
    File.ReadAllLines filePath