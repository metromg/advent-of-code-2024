open System.IO
open System.Text.RegularExpressions

let filePath = "03/input.txt"
let input = File.ReadAllText(filePath)

// Part 1
let processInstructions1 input =
    let regex = Regex(@"mul\((\d{1,3}),(\d{1,3})\)")
    let matches = regex.Matches(input)

    let mulInstructions = matches |> Seq.map (fun m -> (int m.Groups.[1].Value, int m.Groups.[2].Value))

    mulInstructions |> Seq.map (fun (a, b) -> a * b) |> Seq.sum

let solution1 = processInstructions1 input
printfn "Solution 1: %i" solution1

// Part 2
type Instruction =
    | Do
    | Dont
    | Mul

let processInstructions2 input =
    let regex = Regex(@"(?:do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))")
    let matches = regex.Matches(input)

    let instructions =
        matches
        |> Seq.map (fun m ->
            let instruction = m.Value
            match instruction with
            | "do()" -> Do, None
            | "don't()" -> Dont, None
            | _ -> Mul, Some (int m.Groups.[1].Value, int m.Groups.[2].Value)
        )

    let (totalSum, _) =
        instructions
        |> Seq.fold (fun (sum, isEnabled) (instruction, args) ->
            match instruction, args with
            | Do, _ -> (sum, true)
            | Dont, _ -> (sum, false)
            | Mul, Some (a, b) when isEnabled -> (sum + a * b, isEnabled)
            | _ -> (sum, isEnabled)
        ) (0, true)

    totalSum

let solution2 = processInstructions2 input
printfn "Solution 2: %i" solution2