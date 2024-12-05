open System.IO

let filePath = "05/input.txt"
let input = File.ReadAllLines(filePath)

let orderingRules =
    input
    |> Array.filter (fun line -> line.Contains('|'))
    |> Array.map (fun line -> line.Split('|'))
    |> Array.map (fun values -> (int values.[0], int values.[1]))

let updates =
    input
    |> Array.filter (fun line -> line.Contains(','))
    |> Array.map (fun line -> line.Split(','))
    |> Array.map (fun values -> values |> Array.map int)

// Part 1
let isValueAtValidPositionInUpdate (update: int array) (orderingRules: (int * int) array) position =
    let value = update.[position]
    let relevantOrderingRules =
        orderingRules
        |> Array.filter (fun (before, _) -> before = value)

    not (
        update 
        |> Array.take (position) 
        |> Array.exists (fun (value: int) -> relevantOrderingRules |> Array.exists (fun (_, after) -> after = value))
    )

let isUpdateInValidOrder orderingRules update =
    update 
    |> Array.mapi (fun i _ -> i) 
    |> Array.forall (isValueAtValidPositionInUpdate update orderingRules)

let findMiddleValue update =
    let updateLength = update |> Array.length
    let middleIndex = updateLength / 2
    update.[middleIndex]

let result1 =
    updates
    |> Array.filter (isUpdateInValidOrder orderingRules)
    |> Array.map findMiddleValue
    |> Array.sum

printfn "Sum of middle values of valid updates %i" result1

// Part 2
let sortUpdate orderingRules update =
    let precedence a b =
        if orderingRules |> Array.exists (fun (before, after) -> before = a && after = b) then -1
        elif orderingRules |> Array.exists (fun (before, after) -> before = b && after = a) then 1
        else 0
    update |> Array.sortWith precedence

let result2 =
    updates
    |> Array.filter (fun update -> not (isUpdateInValidOrder orderingRules update))
    |> Array.map (sortUpdate orderingRules)
    |> Array.map findMiddleValue
    |> Array.sum

printfn "Sum of middle values of corrected updates %i" result2