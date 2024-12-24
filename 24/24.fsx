open System
open System.IO

let filePath = "24/input.txt"
let input = File.ReadAllLines(filePath)

let wiresInput = input |> Array.takeWhile (fun e -> not (String.IsNullOrWhiteSpace(e)))
let gatesInput = input |> Array.skip ((wiresInput |> Array.length) + 1)

type GateDefinition = string * string * string
type Gate =
    | And of GateDefinition
    | Or of GateDefinition
    | Xor of GateDefinition

let parseWires wiresInput =
    wiresInput
    |> Array.fold (fun map (input: string) ->
        let split = input.Split(": ")
        let name = split.[0]
        let value = int split.[1]
        map |> Map.add name value
    ) Map.empty

let parseGates gatesInput =
    gatesInput
    |> Array.map (fun (input: string) ->
        let split = input.Split(" ") |> Array.filter (fun e -> not (e = "->"))
        let gateType = split.[1]
        let inputA = split.[0]
        let inputB = split.[2]
        let output = split.[3]

        match gateType with
        | "AND" -> And (inputA, inputB, output)
        | "OR" -> Or (inputA, inputB, output)
        | "XOR" -> Xor (inputA, inputB, output)
        | _ -> failwith "Failed to parse gate"
    )
    |> Array.toList

let getWireValue name wires =
    wires |> Map.tryFind name

let setWireValue name value wires =
    wires |> Map.add name value

let gateDefinition =
    (function And def | Or def | Xor def -> def)

let calculate inputA inputB gate =
    match gate with
    | And _ -> inputA &&& inputB
    | Or _ -> inputA ||| inputB
    | Xor _ -> inputA ^^^ inputB

let processGates gates wires =
    Seq.unfold (fun (remainingGates, wires) ->
        match remainingGates with
        | [] -> None
        | _ ->
            let newWires, processedGates =
                remainingGates
                |> Seq.fold (fun (wires, processedGates) gate ->
                    let inputAName, inputBName, outputName = gateDefinition gate
                    let inputA, inputB = (wires |> getWireValue inputAName), (wires |> getWireValue inputBName)
                    
                    match inputA, inputB with
                    | (Some a, Some b) ->
                        let result = gate |> calculate a b
                        wires |> setWireValue outputName result, gate :: processedGates
                    | _ ->
                        wires, processedGates
                ) (wires, [])
            let newRemainingGates = remainingGates |> List.except processedGates
            Some (newWires, (newRemainingGates, newWires))
    ) (gates, wires)
    |> Seq.last

let calculateNumber (key: string) wires =
    let binaryString =
        wires
        |> Map.toList
        |> List.filter (fun ((name: string), _) -> name.StartsWith(key))
        |> List.sortByDescending (fun (name, _) -> name)
        |> List.fold (fun acc (_, value) ->
            acc + string value
        ) ""
    Convert.ToInt64(binaryString, 2)

let simulate gates wires =
    wires |> processGates gates |> calculateNumber "z"

// Part 1
let wires = parseWires wiresInput
let gates = parseGates gatesInput
let output = wires |> simulate gates
printfn "Output number for wires starting with z: %i" output

// Part 2
let wireName character number =
    character + (string number).PadLeft(2, '0')

// Checks if the given wire corresponds to a correctly wired XOR gate for the current bit.
// The XOR gate should take inputs from the X and Y wires of the same bit number.
let checkXor wire number outputToGateMap =
    match outputToGateMap |> Map.tryFind wire with
    | Some (Xor (a, b, _)) ->
        // Check if the inputs to the XOR gate are exactly the X and Y wires for the given bit.
        Set([a ; b]) = Set([wireName "x" number ; wireName "y" number])
    | _ -> false

// Checks if the given wire corresponds to a correctly wired AND gate for X and Y inputs.
// This checks the generation of the carry bit directly from the X and Y inputs.
let checkCarryFromXY wire number outputToGateMap =
    match outputToGateMap |> Map.tryFind wire with
    | Some (And (a, b, _)) ->
        // The AND gate should take inputs from the X and Y wires for the given bit.
        Set([a ; b]) = Set([wireName "x" number ; wireName "y" number])
    | _ -> false

// Recursively checks if the given wire corresponds to a correctly wired AND gate for carry propagation.
// This checks the generation of the carry bit from a previous carry and a partial XOR result.
let rec checkCarryFromCarry wire number outputToGateMap =
    match outputToGateMap |> Map.tryFind wire with
    | Some (And (a, b, _)) ->
        // Validates that one input is a partial XOR and the other is a recursive carry check.
        (checkXor a number outputToGateMap && checkCarry b number outputToGateMap) ||
        (checkXor b number outputToGateMap && checkCarry a number outputToGateMap)
    | _ -> false

// Checks the correctness of the carry wire for a given bit.
// Combines multiple cases: carry from XY (direct inputs) and carry propagation from a previous bit.
and checkCarry wire number outputToGateMap =
    match outputToGateMap |> Map.tryFind wire with
    | Some (And (a, b, _)) when number = 1 ->
        // For the least significant bit, the carry is directly derived from X00 and Y00.
        Set([a ; b]) = Set(["x00" ; "y00"])
    | Some (Or (a, b, _)) ->
        // For other bits, the carry is generated by combining carry from XY and carry propagation.
        (checkCarryFromXY a (number - 1) outputToGateMap && checkCarryFromCarry b (number - 1) outputToGateMap) ||
        (checkCarryFromXY b (number - 1) outputToGateMap && checkCarryFromCarry a (number - 1) outputToGateMap)
    | _ -> false

// Validates the correctness of the Z output wire for a given bit.
// The Z output should be the XOR of the sum inputs and the carry.
let checkZ wire number outputToGateMap =
    match outputToGateMap |> Map.tryFind wire with
    | Some (Xor (a, b, _)) when number = 0 ->
        // For the least significant bit, Z00 is simply the XOR of X00 and Y00.
        Set([a ; b]) = Set(["x00" ; "y00"])
    | Some (Xor (a, b, _)) ->
        // For other bits, Z is derived from the XOR of carry and a partial sum XOR.
        (checkXor a number outputToGateMap && checkCarry b number outputToGateMap) ||
        (checkXor b number outputToGateMap && checkCarry a number outputToGateMap)
    | _ -> false

let check zNumber outputToGateMap =
    let wire = wireName "z" zNumber
    checkZ wire zNumber outputToGateMap

let swapPairs outputToGateMap =
    let nextMismatch outputToGateMap =
        Seq.unfold (fun i ->
            match check i outputToGateMap with
            | true -> Some (i+1, i+1)
            | false -> None
        ) 0
        |> Seq.tryLast
        |> Option.defaultValue 0

    let rec findImprovement outputToGateMap currentMismatch wirePairs =
        match wirePairs with
        | [] -> None
        | (a, b) :: rest ->
            let swappedOutputToGateMap = 
                outputToGateMap
                |> Map.add a (outputToGateMap.[b])
                |> Map.add b (outputToGateMap.[a])

            if nextMismatch swappedOutputToGateMap > currentMismatch then
                Some (swappedOutputToGateMap, a, b)
            else
                findImprovement outputToGateMap currentMismatch rest

    let rec loop swaps iterations outputToGateMap =
        if iterations > 0 then
            let currentMismatch = nextMismatch outputToGateMap
            let outputWires = outputToGateMap |> Map.keys |> List.ofSeq
            let wirePairs = 
                outputWires 
                |> List.allPairs outputWires
                |> List.filter (fun (a, b) -> a <> b)

            match findImprovement outputToGateMap currentMismatch wirePairs with
            | Some (updatedOutputToGateMap, a, b) ->
                loop ((a, b) :: swaps) (iterations - 1) updatedOutputToGateMap
            | None -> swaps
        else
            swaps

    loop [] 4 outputToGateMap

let necessarySwaps gates =
    let outputToGateMap =
        gates
        |> List.map (fun gate ->
            let (_, _, output) = gateDefinition gate
            (output, gate)
        )
        |> Map.ofList

    swapPairs outputToGateMap

let swapsCommaseparated swaps =
    let sorted =
        swaps
        |> Seq.collect (fun (a, b) -> [a ; b])
        |> Seq.sort
    String.Join(",", sorted)

let swapWires = gates |> necessarySwaps |> swapsCommaseparated
printfn "Wires involved in necessary swaps: %s" swapWires