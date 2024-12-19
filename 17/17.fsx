open System.IO

let filePath = "17/input.txt"
let input = File.ReadAllLines(filePath)

type RegisterRef = A | B | C

type LiteralOperand = int64
type ComboOperand = LiteralOperand of int64 | RefOperand of RegisterRef

type Instruction =
    | Adv of ComboOperand
    | Bxl of LiteralOperand
    | Bst of ComboOperand
    | Jnz of LiteralOperand
    | Bxc
    | Out of ComboOperand
    | Bdv of ComboOperand
    | Cdv of ComboOperand

let parseRegisterValue registerRef (inputLine: string) =
    (registerRef, inputLine.Split(':').[1].Trim() |> int64)

let parseInstructions (inputLine: string) =
    let parseComboOperand operand =
        match operand with
        | 4 -> RefOperand A
        | 5 -> RefOperand B
        | 6 -> RefOperand C
        | value -> LiteralOperand value

    inputLine.Split(':').[1].Trim().Split(',')
    |> Array.map int
    |> Array.pairwise
    |> Array.mapi (fun i pair -> i % 2 = 0, pair)
    |> Array.filter fst
    |> Array.map snd
    |> Array.map (fun (opcode, operand) ->
        match opcode with
        | 0 -> Adv (parseComboOperand operand)
        | 1 -> Bxl operand
        | 2 -> Bst (parseComboOperand operand)
        | 3 -> Jnz operand
        | 4 -> Bxc
        | 5 -> Out (parseComboOperand operand)
        | 6 -> Bdv (parseComboOperand operand)
        | 7 -> Cdv (parseComboOperand operand)
        | _ -> failwith "Invalid opcode"
    )

let registerA = parseRegisterValue A input.[0]
let registerB = parseRegisterValue B input.[1]
let registerC = parseRegisterValue C input.[2]
let registers = Map ([ registerA ; registerB ; registerC ])

let instructions = parseInstructions input.[4]

// Part 1
let rightShift (a: int64) (b: int64) : int64 =
    if b >= 64L then
        0L
    elif b < 0L then
        invalidArg "b" "Shift amount cannot be negative"
    else
        a >>> (int b)

let processInstructions registers instructions =
    Seq.unfold (fun (instructionPointer, registers, outputs) ->
        let instruction = instructions |> Array.tryItem instructionPointer
        let nextInstructionPointer = instructionPointer + 1

        let comboOperandValue operand =
            match operand with
            | LiteralOperand value -> value
            | RefOperand registerRef -> registers |> Map.find registerRef

        match instruction with
        | Some (Adv operand) -> 
            let a = registers |> Map.find A
            let result = rightShift a (comboOperandValue operand)
            let newRegisters = registers |> Map.add A result
            Some (outputs, (nextInstructionPointer, newRegisters, outputs))
        | Some (Bxl operand) ->
            let b = registers |> Map.find B
            let result = b ^^^ operand
            let newRegisters = registers |> Map.add B result
            Some (outputs, (nextInstructionPointer, newRegisters, outputs))
        | Some (Bst operand) ->
            let result = (comboOperandValue operand) % 8L
            let newRegisters = registers |> Map.add B result
            Some (outputs, (nextInstructionPointer, newRegisters, outputs))
        | Some (Jnz operand) ->
            let a = registers |> Map.find A
            let newInstructionPointer = if a = 0 then nextInstructionPointer else int operand
            Some (outputs, (newInstructionPointer, registers, outputs))
        | Some Bxc ->
            let b = registers |> Map.find B
            let c = registers |> Map.find C
            let result = b ^^^ c
            let newRegisters = registers |> Map.add B result
            Some (outputs, (nextInstructionPointer, newRegisters, outputs))
        | Some (Out operand) ->
            let result = (comboOperandValue operand) % 8L
            let newOutputs = outputs @ [result]
            Some (newOutputs, (nextInstructionPointer, registers, newOutputs))
        | Some (Bdv operand) ->
            let a = registers |> Map.find A
            let result = rightShift a (comboOperandValue operand)
            let newRegisters = registers |> Map.add B result
            Some (outputs, (nextInstructionPointer, newRegisters, outputs))
        | Some (Cdv operand) ->
            let a = registers |> Map.find A
            let result = rightShift a (comboOperandValue operand)
            let newRegisters = registers |> Map.add C result
            Some (outputs, (nextInstructionPointer, newRegisters, outputs))
        | None -> None
    ) (0, registers, [])
    |> Seq.last

let outputs =
    processInstructions registers instructions
    |> List.map string
    |> String.concat ","

printfn "Outputs: %s" outputs

// Part 2 (😵‍💫)

// Manually reverse engineered program: 2,4 1,4 7,5 4,1 1,4 5,5 0,3 3,0
// 2,4 -> b = a % 8
// 1,4 -> b = b ^^^ 4
// 7,5 -> c = a >>> b
// 4,1 -> b = b ^^^ c
// 1,4 -> b = b ^^^ 4
// 5,5 -> out b % 8
// 0,3 -> a = a >>> 3
// 3,0 -> jump 0 when a!=0

let rec find target answer =
    match target with
    | [] -> Some answer
    | _ ->
        [0L..7L]
        |> List.tryPick (fun t ->
            let mutable a = (answer <<< 3) ||| t
            if a = 0 then
                a <- 16 // wtf
            let mutable b = a % 8L
            b <- b ^^^ 4L
            let c = rightShift a b
            b <- b ^^^ c
            b <- b ^^^ 4L
            if (b % 8L) = (target |> List.last) then
                find (target |> List.take ((target |> List.length) - 1)) a
            else
                None
        )

let targetProgram = input.[4].Split(':').[1].Trim().Split(',') |> Array.map int64 |> Array.toList
let lowestA = find targetProgram 0

printfn "Lowest A: %i" (lowestA |> Option.defaultValue 0)