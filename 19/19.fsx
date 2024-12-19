open System
open System.IO

let filePath = "19/input.txt"
let input = File.ReadAllLines(filePath)

let patterns = input.[0].Split(", ", StringSplitOptions.RemoveEmptyEntries)
let designs = input |> Array.skip 2

let countWays patterns design =
    let patternLengths = patterns |> Seq.map String.length
    let minPatternLength = patternLengths |> Seq.min
    let maxPatternLength = patternLengths |> Seq.max

    let patternSet = patterns |> Set.ofSeq

    let rec findWays remaining cache =
        match cache |> Map.tryFind remaining with
        | Some result ->
            result, cache
        | None ->
            match remaining with
            | "" ->
                1L, cache |> Map.add remaining 1L
            | _ ->
                let maxCheck = Math.Min(maxPatternLength, remaining.Length)
                let lengths = [maxCheck .. -1 .. minPatternLength]
                
                let total, newCache =
                    lengths
                    |> List.fold (fun (acc, currentCache) len ->
                        let patternToFind = remaining[0 .. (len - 1)]
                        if patternSet |> Set.contains patternToFind then
                            let ways, updatedCache = findWays (remaining.[len..]) currentCache
                            acc + ways, updatedCache
                        else
                            acc, currentCache
                    ) (0L, cache)

                total, newCache |> Map.add remaining total

    findWays design Map.empty |> fst

// Part 1
let numberOfPossibleDesigns =
    designs
    |> Array.filter (fun design -> (design |> countWays patterns) > 0L)
    |> Array.length

printfn "Number of possible designs: %i" numberOfPossibleDesigns

// Part 2
let totalNumberOfWays =
    designs
    |> Array.sumBy (countWays patterns)

printfn "Total number of ways: %i" totalNumberOfWays