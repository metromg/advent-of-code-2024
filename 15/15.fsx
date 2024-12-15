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

type MoveResult =
    | Moved of ((int * int) * Map<(int * int), MapElement>)
    | Blocked of ((int * int) * Map<(int * int), MapElement>)

let toTuple =
    function Moved (newPos, newMap) | Blocked (newPos, newMap) -> (newPos, newMap)

let rec move pos dir map =
    let step dir (x, y) =
        match dir with
        | Up -> (x, y - 1)
        | Down -> (x, y + 1)
        | Left -> (x - 1, y)
        | Right -> (x + 1, y)

    let applyWhenNotBlocked movers =
        movers
        |> List.fold (fun currentResult mover ->
            match currentResult with
            | Moved (_, currentMap) -> mover currentMap
            | Blocked _ -> Blocked (pos, map)
        ) (Moved (pos, map))

    let posInDir = pos |> step dir
    let mapElemInDir = map |> Map.tryFind posInDir

    match mapElemInDir with
    | Some (Box) ->
        applyWhenNotBlocked [
            (fun map -> map |> move posInDir dir) ;
            (fun map -> map |> move pos dir)
        ]
    | Some (BoxLeft | BoxRight as boxElem) ->
        applyWhenNotBlocked [
            (fun map ->
                let connectedBoxPos (x, y) boxElem =
                    match boxElem with
                    | BoxLeft -> (x + 1, y)
                    | BoxRight -> (x - 1, y)
                    | _ -> failwith "Expected BoxLeft or BoxRight"

                map |> move (connectedBoxPos posInDir boxElem) dir
            ) ;
            (fun map -> map |> move posInDir dir) ;
            (fun map -> map |> move pos dir)
        ]
    | Some _ ->
        Blocked (pos, map)
    | None ->
        let mapElement = map |> Map.find pos
        let updatedMap =
            map
            |> Map.remove pos
            |> Map.add posInDir mapElement
        Moved (posInDir, updatedMap)

let processMoves moves startPos map =
    moves
    |> Array.fold (fun (pos, map) dir ->
        map
        |> move pos dir
        |> toTuple
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