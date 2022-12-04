
module AdventOfCode._04_12.Program 

let loadFile fileName = System.IO.File.ReadLines fileName

let private createSection (sectionShortHand :string) =
    match (sectionShortHand.Split "-") with
    | [|var1 ; var2|] -> [| (int var1) .. (int var2)|]
    | _ ->  [|  |]

let private parseToTuple (line:string) =
    match (line.Split ",") with
    | [|var1 ; var2|] -> Some ( createSection var1   ,  createSection var2 )
    | _ -> None
    
let private intersectPairs pair predicate =
    match pair with
    | None -> false
    | Some x ->
        let f = Set.ofArray (fst x)
        let s = Set.ofArray (snd x)
        let r = Set.intersect f s
        
        predicate r.Count f.Count s.Count
    
let private doPairSectionsContainEachOther pair =
    intersectPairs pair (fun intersectCnt firstPairCnt secondPairCount ->  intersectCnt = firstPairCnt  || intersectCnt = secondPairCount )
        
let private doPairSectionsIntersect pair =
    intersectPairs pair (fun intersectCnt _ _ ->  intersectCnt > 0)

let part1 (lines:string seq) =
   let r = lines
            |> Seq.map parseToTuple
            |> Seq.map doPairSectionsContainEachOther
            |> Seq.filter (fun x -> x = true)
   (List.ofSeq r).Length
            
let part2 (lines:string seq) =
   let r = lines
            |> Seq.map parseToTuple
            |> Seq.map doPairSectionsIntersect
            |> Seq.filter (fun x -> x = true)
   (List.ofSeq r).Length
            

