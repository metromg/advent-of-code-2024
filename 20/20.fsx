open System.IO

let filePath = "20/input.txt"
let input = File.ReadAllLines(filePath)

type GridElement = Wall | Path | Start | End

let parseGrid (input: string array) =
    let parseElement = function
        | '#' -> Wall
        | '.' -> Path
        | 'S' -> Start
        | 'E' -> End
        | _ -> failwith "Invalid grid element"

    let grid =
        input
        |> Array.map (fun row -> row.ToCharArray() |> Array.map parseElement)

    let findPosition element =
        grid
        |> Array.mapi (fun rowIndex row ->
            row |> Array.mapi (fun colIndex value ->
                (rowIndex, colIndex, value)
            )
        )
        |> Array.concat
        |> Array.filter (fun (_, _, e) -> e = element)
        |> Array.map (fun (y, x, _) -> (y, x))
        |> Array.exactlyOne

    findPosition Start, findPosition End, grid

let isInBounds (y, x) (grid: GridElement array array) =
    0 <= y && y < grid.Length && 0 <= x && x < grid.[0].Length

let neighbors (y, x) =
    [ (y + 1, x) ; (y - 1, x) ; (y, x + 1) ; (y, x - 1) ]

let manhattanDistance (ay, ax) (by, bx) =
    abs (ay - by) + abs (ax - bx)

let findPathDistances startPos endPos grid =
    Seq.unfold (fun (queue, distances) ->
        match queue with
        | [] -> None
        | (position, distance) :: poppedQueue ->
            if distances |> Map.containsKey position then
                Some (distances, (poppedQueue, distances))
            elif position = endPos then
                let newDistances = distances |> Map.add position distance
                Some (newDistances, (poppedQueue, newDistances))
            else
                let pathNeighbors =
                    position
                    |> neighbors
                    |> List.filter (fun (ny, nx) -> grid |> isInBounds (ny, nx) && grid.[ny][nx] <> Wall)
                let newQueue = pathNeighbors |> List.fold (fun q n -> q @ [(n, distance + 1)]) poppedQueue
                let newDistances = distances |> Map.add position distance
                Some (newDistances, (newQueue, newDistances))
    ) ([(startPos, 0)], Map.empty)
    |> Seq.last

let findNumberOfCheats maxCheatDistance minSaving distanceMap =
    let distances = distanceMap |> Map.toArray
    let n = distances |> Array.length
    
    let pairs =
        Seq.init (n - 1) (fun i ->
            Seq.init (n - i) (fun j -> distances.[i], distances.[i + j])
        )
        |> Seq.concat

    pairs
    |> Seq.filter (fun ((posA, distA), (posB, distB)) ->
        let cheatDistance = manhattanDistance posA posB
        let skippedPathDistance = abs (distB - distA)
        cheatDistance <= maxCheatDistance && skippedPathDistance >= cheatDistance + minSaving)
    |> Seq.length

let startPos, endPos, grid = parseGrid input
let pathDistances = grid |> findPathDistances startPos endPos

// Part 1
let numberOfCheats2 = pathDistances |> findNumberOfCheats 2 100
printfn "Number of cheats with max distance 2: %i" numberOfCheats2

// Part 2
let numberOfCheats20 = pathDistances |> findNumberOfCheats 20 100
printfn "Number of cheats with max distance 20: %i" numberOfCheats20