open System
open System.IO

let readColumnsFromFile filePath =
    let lines = File.ReadAllLines(filePath)
    let column1, column2 =
        lines
        |> Array.fold (fun (col1, col2) line ->
            let parts = line.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)
            if parts.Length = 2 then
                (Int32.Parse(parts.[0]) :: col1, Int32.Parse(parts.[1]) :: col2)
            else
                (col1, col2)
        ) ([], [])
    (List.rev column1, List.rev column2)

let filePath = "01/input.txt"
let column1, column2 = readColumnsFromFile filePath

let countOccurrences number list =
    list |> List.filter (fun x -> x = number) |> List.length

let similarityScore =
    column1
    |> List.map (fun number ->
        let count = countOccurrences number column2
        number * count
    )
    |> List.sum

printfn "Similarity Score: %d" similarityScore