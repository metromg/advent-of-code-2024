open System
open System.IO

let filePath = "15/input.txt"
let input = File.ReadAllText(filePath)

let splitInput = input.Split([|Environment.NewLine + Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)

type MapElement = Wall | Box | BoxLeft | BoxRight | Robot

let parseMap (mapInput: string) =
    mapInput.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
    |> Array.mapi (fun rowIndex row ->
        row.ToCharArray()
        |> Array.indexed
        |> Array.filter (fun (_, value) -> value = '#' || value = 'O' || value = '[' || value = ']' || value = '@')
        |> Array.map (fun (colIndex, value) ->
            let mapElement = 
                match value with
                | '#' -> Wall
                | 'O' -> Box
                | '[' -> BoxLeft
                | ']' -> BoxRight
                | '@' -> Robot
                | _ -> failwith "Failed to parse map"

            (colIndex, rowIndex), mapElement
        )
    )
    |> Array.concat
    |> Map.ofArray

let findStartPos map =
    map |> Map.findKey (fun _ e -> e = Robot)

type Direction = Up | Down | Left | Right

let parseMoves (movesInput: string) =
    movesInput.ToCharArray()
    |> Array.filter (fun value -> value = '^' ||  value = 'v' || value = '<' || value = '>')
    |> Array.map (fun value ->
        match value with
        | '^' -> Up
        | 'v' -> Down
        | '<' -> Left
        | '>' -> Right
        | _ -> failwith "Failed to parse moves"
    )

let rec move pos dir map =
    let step dir (x, y) =
        match dir with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    let attemptMove prevPosOfPrevMove newPosOfPrevMove fallback next =
        if prevPosOfPrevMove <> newPosOfPrevMove then next()
        else fallback

    let handleBoxMovement boxPos map fallback next =
        let (newBoxPos, updatedMap) = map |> move boxPos dir
        attemptMove boxPos newBoxPos fallback (fun () -> next updatedMap)

    let handleConnectedBoxMovement boxPos boxElem map fallback next =
        let getConnectedBoxPos (x, y) = function
            | BoxLeft -> (x + 1, y)
            | BoxRight -> (x - 1, y)
            | _ -> failwith "Expected BoxLeft or BoxRight"

        let connectedBoxPos = getConnectedBoxPos boxPos boxElem

        let (newConnectedBoxPos, updatedMap) = map |> move connectedBoxPos dir
        attemptMove connectedBoxPos newConnectedBoxPos fallback (fun () ->
            handleBoxMovement boxPos updatedMap fallback next
        )

    let moveElement currentPos newPos map =
        let mapElement = map |> Map.find currentPos
        let updatedMap =
            map
            |> Map.remove currentPos
            |> Map.add newPos mapElement
        (newPos, updatedMap)

    let posInDir = pos |> step dir
    let mapElementInDir = map |> Map.tryFind posInDir

    match mapElementInDir with
    | Some (Box) ->
        handleBoxMovement posInDir map (pos, map) (fun updatedMap ->
            updatedMap |> move pos dir
        )
    | Some (BoxLeft | BoxRight as connectedBox) ->
        handleConnectedBoxMovement posInDir connectedBox map (pos, map) (fun updatedMap ->
            updatedMap |> move pos dir
        )
    | Some _ ->
        (pos, map)
    | None ->
        map |> moveElement pos posInDir

let processMoves moves startPos map =
    moves
    |> Array.fold (fun (pos, map) dir ->
        map |> move pos dir
    ) (startPos, map)
    |> snd

let sumBoxGpsCoords map =
    map
    |> Map.toArray
    |> Array.filter (fun (_, elem) -> elem = Box || elem = BoxLeft)
    |> Array.sumBy (fun ((x, y), _) -> x + (y * 100))

let calculateSumOfFinalBoxGpsCoords moves startPos map =
    map
    |> processMoves moves startPos
    |> sumBoxGpsCoords

let moves = parseMoves (splitInput.[1])

// Part 1
let map = parseMap (splitInput.[0])
let startPos = map |> findStartPos

let boxesGpsSum = map |> calculateSumOfFinalBoxGpsCoords moves startPos
printfn "GPS sum of boxes: %i" boxesGpsSum

// Part 2
let widen (mapInput: string) =
    let replace (oldValue: string) (newValue: string) (input: string) =
        input.Replace(oldValue, newValue)

    mapInput
    |> replace "#" "##"
    |> replace "O" "[]"
    |> replace "." ".."
    |> replace "@" "@."

let wideMap = parseMap (widen splitInput.[0])
let wideStartPos = wideMap |> findStartPos

let wideBoxesGpsSum = wideMap |> calculateSumOfFinalBoxGpsCoords moves wideStartPos
printfn "GPS sum of wide boxes: %i" wideBoxesGpsSum