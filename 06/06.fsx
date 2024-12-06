open System.IO

let filePath = "06/input.txt"
let input = File.ReadAllLines(filePath)

type Direction =
    | Up
    | Right
    | Down
    | Left

type MapElement =
    | Obstacle
    | Unvisited
    | Visited of Direction list

type Guard = Direction * (int * int)

let inputValuesWithPositions =
    input
    |> Array.mapi (fun rowIndex row -> 
        row.ToCharArray() |> Array.mapi (fun colIndex value -> value, (colIndex, rowIndex))
    )
    |> Array.concat

let map =
    inputValuesWithPositions
    |> Array.map (fun (value, position) ->
        match value with
        | '#' -> (position, Obstacle)
        | _ -> (position, Unvisited)
    )
    |> Map.ofArray

let (_, guardStartingPosition) =
    inputValuesWithPositions
    |> Array.find (fun (value, _) -> value = '^')

let guard = Guard(Up, guardStartingPosition)

// Part 1
let isInBounds position map =
    map |> Map.containsKey position

let isObstacleAt position map =
    (isInBounds position map) && (map |> Map.find position) = Obstacle

let isVisited mapElement =
    match mapElement with
    | Visited _ -> true
    | _ -> false

let visit direction mapElement =
    match mapElement with
    | Visited directions -> Visited(direction :: directions)
    | _ -> Visited([direction])

let visitPositionInDirection position direction map =
    map |> Map.change position (fun mapElement ->
        match mapElement with
        | Some e -> Some (e |> visit direction)
        | None -> None
    )

let direction (guard: Guard) =
    fst guard
let position (guard: Guard) =
    snd guard

let stepForward (guard: Guard) : Guard =
    match guard with
    | (Up, (x, y)) -> (Up, (x, y - 1))
    | (Right, (x, y)) -> (Right, (x + 1, y))
    | (Down, (x, y)) -> (Down, (x, y + 1))
    | (Left, (x, y)) -> (Left, (x - 1, y))

let turnAround (guard: Guard) : Guard =
    match guard with
    | (Up, pos) -> (Right, pos)
    | (Right, pos) -> (Down, pos)
    | (Down, pos) -> (Left, pos)
    | (Left, pos) -> (Up, pos)

let processNextStep map guard =
    let guardAtNextPos = guard |> stepForward

    let pos = guard |> position
    let nextPos = guardAtNextPos |> position

    if not (map |> isInBounds pos) then None
    elif map |> isObstacleAt nextPos then Some (map, guard |> turnAround)
    else Some (map |> visitPositionInDirection pos (guard |> direction), guardAtNextPos)

let numberOfVisitedPositionsWhenReachingBounds =
    Seq.unfold (fun (currentMap, currentGuard) ->
        match processNextStep currentMap currentGuard with
        | Some (newMap, newGuard) -> Some ((newMap, newGuard), (newMap, newGuard))
        | None -> None
    ) (map, guard)
    |> Seq.last
    |> fst
    |> Map.filter (fun _ value -> isVisited value)
    |> Map.count

printfn "Number of visited positions: %i" numberOfVisitedPositionsWhenReachingBounds

// Part 2
let addObstacle position map =
    map |> Map.change position (fun _ -> Some Obstacle)

let positionIsAlreadyVisitedInDirection position direction map =
    let mapElement = map |> Map.tryFind position
    match mapElement with
    | Some (Visited directions) -> directions |> List.contains direction
    | _ -> false

let numberOfObstaclesProducingLoops =
    map
    |> Map.toList
    |> List.map (fun (pos, _) -> map |> addObstacle pos)
    |> List.map (fun mapVariation ->
        let mutable isCancelledBecauseOfLoop = false

        Seq.unfold (fun (currentMap, currentGuard) ->
            let currentPosition = currentGuard |> position
            let currentDirection = currentGuard |> direction

            if currentMap |> positionIsAlreadyVisitedInDirection currentPosition currentDirection then
                isCancelledBecauseOfLoop <- true
                None
            else
                match processNextStep currentMap currentGuard with
                | Some (newMap, newGuard) -> Some ((newMap, newGuard), (newMap, newGuard))
                | None -> None
        ) (mapVariation, guard)
        |> Seq.toArray
        |> ignore

        isCancelledBecauseOfLoop
    )
    |> List.filter (fun isCancelledBecauseOfLoop -> isCancelledBecauseOfLoop)
    |> List.length

printfn "Number of obstacles producing loops: %i" numberOfObstaclesProducingLoops