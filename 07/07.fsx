open System
open System.IO

let filePath = "07/input.txt"
let input = File.ReadAllLines(filePath)

let equations =
    input
    |> Array.map (fun line ->
        let splitLine = line.Split(':')
        let testValue = int64 (splitLine.[0])
        let values = splitLine.[1].Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int64

        (testValue, values)
    )

type Operator =
    | Add
    | Multiply
    | Concat

let apply a b operator =
    match operator with
    | Add -> a + b
    | Multiply -> a * b
    | Concat -> int64 ((string a) + (string b))

let possibleOperatorCombinations length possibleOperators =
    List.init length (fun _ -> possibleOperators)
    |> List.fold (fun combinations operators ->
        combinations |> List.collect (fun combination -> operators |> List.map (fun op -> combination @ [op]))
    ) [[]]

let testEquation possibleOperators (testValue, values) =
    let numberOfOperatorsNeeded = (values |> Array.length) - 1
    let results =
        possibleOperators |> possibleOperatorCombinations numberOfOperatorsNeeded
        |> List.map (fun operators ->
            values
            |> Array.skip 1
            |> Array.indexed
            |> Array.fold (fun result (i, value) ->
                operators.[i] |> apply result value
            ) values.[0]
        )

    results |> List.contains testValue

// Part 1
let sumOfValidTestValuesWithAddOrMultiply =
    equations
    |> Array.filter (testEquation [ Add ; Multiply ])
    |> Array.sumBy fst

printfn "Sum of valid test values with add or multiply %i" sumOfValidTestValuesWithAddOrMultiply

// Part 2
let sumOfValidTestValuesWithAddMultiplyOrConcat =
    equations
    |> Array.filter (testEquation [ Add ; Multiply ; Concat ])
    |> Array.sumBy fst

printfn "Sum of valid test values with add, multiply or concat %i" sumOfValidTestValuesWithAddMultiplyOrConcat