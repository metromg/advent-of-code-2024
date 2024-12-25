open System
open System.IO

let filePath = "25/input.txt"
let input = File.ReadAllLines(filePath)

let parseInput input =
    input
    |> Array.chunkBySize 8
    |> Array.fold (fun (keys, locks) chunk ->
        let keyOrLockRows = chunk |> Array.filter (fun row -> not (String.IsNullOrWhiteSpace(row)))
        let isLock = keyOrLockRows.[0] = "#####"

        let heights =
            keyOrLockRows
            |> Array.map (fun row -> row.ToCharArray())
            |> Array.transpose
            |> Array.map (fun col -> (col |> Array.filter (fun v -> v = '#') |> Array.length) - 1)
        
        if isLock then
            (keys, heights :: locks)
        else
            (heights :: keys, locks)
    ) ([], [])

let findMatches keys locks =
    Seq.allPairs keys locks
    |> Seq.filter (fun (key, loc) ->
        Seq.zip key loc |> Seq.forall (fun (k, l) -> k + l < 6)
    )

let keys, locks = parseInput input
let numberOfMatches = findMatches keys locks |> Seq.length
printfn "Number of matching key/lock pairs: %i" numberOfMatches