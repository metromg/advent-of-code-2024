open System.IO

let filePath = "21/input.txt"
let input = File.ReadAllLines(filePath)

type NumericButton = Seven | Eight | Nine | Four | Five | Six | One | Two | Three | Zero | ActivateNumeric

let numericPad = Map.ofList [
    (Seven, (0, 0)) ;
    (Eight, (1, 0)) ;
    (Nine, (2, 0)) ;
    (Four, (0, 1)) ;
    (Five, (1, 1)) ;
    (Six, (2, 1)) ;
    (One, (0, 2)) ;
    (Two, (1, 2)) ;
    (Three, (2, 2)) ;
    (Zero, (1, 3)) ;
    (ActivateNumeric, (2, 3))
]

type DirectionalButton = Up | ActivateDirectional | Left | Down | Right

let directionalPad = Map.ofList [
    (Up, (1, 0)) ;
    (ActivateDirectional, (2, 0)) ;
    (Left, (0, 1)) ;
    (Down, (1, 1)) ;
    (Right, (2, 1))
]

let parseNumericSeq (code: string) =
    code.ToCharArray()
    |> Array.map (fun value ->
        match value with
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '1' -> One
        | '2' -> Two
        | '3' -> Three
        | '0' -> Zero
        | 'A' -> ActivateNumeric
        | _ -> failwith "Failed to parse numeric code"
    )
    |> Array.toList

let precalculatePadSeqs keypad =
    let positions = keypad |> Map.toList |> List.map snd

    Seq.allPairs positions positions
    |> Seq.fold (fun possibleSeqsPerButtonPair (posA, posB) ->
        if posA = posB then
            possibleSeqsPerButtonPair |> Map.add (posA, posB) [[ActivateDirectional]]
        else
            let possibilites =
                Seq.unfold (fun (queue, optimal, possibilities) ->
                    match queue with
                    | [] -> None
                    | ((x, y), moves) :: rest ->
                        let potentialNewOptimal = List.length moves + 1
                        let neighbors =
                            [ (x - 1, y, Left) ; (x + 1, y, Right) ; (x, y - 1, Up) ; (x, y + 1, Down) ]
                            |> List.filter (fun (nx, ny, _) -> positions |> List.contains (nx, ny))

                        let newQueue, newOptimal, newPossibilities =
                            neighbors |> List.fold (fun (q, opt, poss) (nx, ny, nm) ->
                                match (nx, ny) with
                                | pos when pos = posB ->
                                    if opt < potentialNewOptimal then
                                        (q, opt, poss)
                                    else
                                        (q, potentialNewOptimal, (moves @ [nm] @ [ActivateDirectional]) :: poss)
                                | _ ->
                                    (q @ [((nx, ny), moves @ [nm])], opt, poss)
                            ) (rest, optimal, possibilities)

                        let foundOptimalPaths = neighbors |> List.exists (fun (nx, ny, _) -> (nx, ny) = posB && optimal < potentialNewOptimal)

                        Some (newPossibilities, ((if foundOptimalPaths then List.empty else newQueue), newOptimal, newPossibilities))
                ) ([(posA, [])], System.Int32.MaxValue, [])
                |> Seq.last
            possibleSeqsPerButtonPair |> Map.add (posA, posB) possibilites
    ) Map.empty

let possibleDirectionalSeqsForNumericKeypad keypad possibleKeypadSeqs sourceSeq =
    let rec cartesianProduct lists =
        match lists with
        | [] -> [[]]
        | head :: tail ->
            let tailProduct = cartesianProduct tail
            head |> List.collect (fun h -> tailProduct |> List.map (fun t -> h :: t))

    sourceSeq
    |> List.pairwise
    |> List.map (fun (source, target) ->
        let sourcePos = keypad |> Map.find source
        let targetPos = keypad |> Map.find target

        possibleKeypadSeqs |> Map.find (sourcePos, targetPos)
    )
    |> cartesianProduct
    |> List.map (fun possibility -> List.concat possibility)

let shortestLengthOfSeqInDepth keypad keypadSeqs (keypadSeqLengths: Map<(int * int) * (int * int), int64>) depth cache seq =
    let rec compute depth cache seq =
        if cache |> Map.containsKey (depth, seq) then
            cache |> Map.find (depth, seq), cache
        else
            let pairs =
                ActivateDirectional :: seq
                |> List.pairwise
                |> List.map (fun (source, target) ->
                    let sourcePos = keypad |> Map.find source
                    let targetPos = keypad |> Map.find target

                    (sourcePos, targetPos)
                )
            
            if depth = 1 then
                let result =
                    pairs
                    |> List.map (fun (sourcePos, targetPos) ->
                        keypadSeqLengths |> Map.find (sourcePos, targetPos)
                    )
                    |> List.sum
                result, (cache |> Map.add (depth, seq) result)
            else
                let result, newCache =
                    pairs
                    |> List.fold (fun (acc, currentCache) (sourcePos, targetPos) ->
                        let possibleSubSeqs = keypadSeqs |> Map.find (sourcePos, targetPos)
                        let possibleSubSeqLengths, newCache =
                            possibleSubSeqs
                            |> List.fold (fun (rs, c) seq ->
                                let r, newc = compute (depth - 1) c seq
                                r :: rs, newc
                            ) ([], currentCache)
                        acc + (possibleSubSeqLengths |> List.min), newCache
                    ) (0L, cache)
                result, (newCache |> Map.add (depth, seq) result)

    compute depth cache seq

let lengthOfShortestDirectionalSeqThroughKeypadChain numericPad possibleNumericPadSeqs directionalPad (possibleDirectionalPadSeqs: Map<(int * int) * (int * int), DirectionalButton list list>) depth numericCode =
    let directionalPadSeqLengths = possibleDirectionalPadSeqs |> Map.map (fun _ possibleSeqs -> int64 (possibleSeqs.[0] |> List.length))
    ActivateNumeric :: parseNumericSeq numericCode
    |> possibleDirectionalSeqsForNumericKeypad numericPad possibleNumericPadSeqs
    |> List.fold (fun (results, cache) seq ->
        let result, newCache = shortestLengthOfSeqInDepth directionalPad possibleDirectionalPadSeqs directionalPadSeqLengths depth cache seq
        result :: results, newCache
    ) ([], Map.empty)
    |> fst
    |> List.min

let calculateTotalComplexities numericPad possibleNumericPadSeqs directionalPad possibleDirectionalPadSeqs depth numericCodes =
    numericCodes
    |> Seq.map (fun numericCode ->
        let directionalSeqLength = lengthOfShortestDirectionalSeqThroughKeypadChain numericPad possibleNumericPadSeqs directionalPad possibleDirectionalPadSeqs depth numericCode
        let numericPart = int64 (numericCode.Substring(0, numericCode.Length - 1))
        directionalSeqLength * numericPart
    )
    |> Seq.sum

let numericPadSeqs = precalculatePadSeqs numericPad
let directionalPadSeqs = precalculatePadSeqs directionalPad

// Part 1
let totalComplexity2 = calculateTotalComplexities numericPad numericPadSeqs directionalPad directionalPadSeqs 2 input
printfn "Total complexity for depth 2 %i" totalComplexity2

// Part 2
let totalComplexity25 = calculateTotalComplexities numericPad numericPadSeqs directionalPad directionalPadSeqs 25 input
printfn "Total complexity for depth 25 %i" totalComplexity25