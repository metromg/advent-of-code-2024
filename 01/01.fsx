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

// Part 1
let sortedList1 = List.sort column1
let sortedList2 = List.sort column2

let differences = 
    List.map2 (fun x y -> abs (x - y)) sortedList1 sortedList2

let totalDifference = List.sum differences

printfn "Total Difference: %d" totalDifference

// Part 2
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