open System.IO

let filePath = "18/input.txt"
let input = File.ReadAllLines(filePath)

let size = 71
let startPos = (0, 0)
let endPos = (70, 70)
let coordinates =
    input
    |> Array.map (fun row ->
        let split = row.Split(',') |> Array.map int
        (split.[0], split.[1])
    )

type GridElement = Corrupted | Safe

let isInBounds (x, y) grid =
    0 <= x && x < (grid |> Array2D.length1) && 0 <= y && y < (grid |> Array2D.length2)

let gridFromCoordinates width height coordinates =
    Array2D.init width height (fun x y ->
        if coordinates |> Seq.contains (x, y) then
            Corrupted
        else
            Safe
    )

let findShortestDistance startPos endPos grid =
    let positionWithDistance =
        Seq.unfold (fun (queue, visited) ->
            match queue with
            | [] -> None
            | ((x, y), currentDistance) :: poppedQueue ->
                let unvisitedSafeNeighbors =
                    [(x + 1, y) ; (x - 1, y) ; (x, y + 1) ; (x, y - 1)]
                    |> List.filter (fun n -> grid |> isInBounds n)
                    |> List.filter (fun (nx, ny) -> grid.[nx, ny] = Safe)
                    |> List.filter (fun n -> not (visited |> Set.contains n))

                let newQueue = unvisitedSafeNeighbors |> List.fold (fun queue n -> queue @ [(n, currentDistance + 1)]) poppedQueue
                let newVisited = unvisitedSafeNeighbors |> List.fold (fun visited n -> visited |> Set.add n) visited

                Some(((x, y), currentDistance), (newQueue, newVisited))
        ) ([(startPos, 0)], Set([startPos]))
        |> Seq.tryFind (fun (pos, _) -> pos = endPos)
        
    match positionWithDistance with
    | Some (_, distance) -> Some distance
    | None -> None

// Part 1
let shortestDistance =
    (coordinates |> Array.take 1024)
    |> gridFromCoordinates size size
    |> findShortestDistance startPos endPos

printfn "Shortest distance: %i" (shortestDistance |> Option.defaultValue 0)

// Part 2
let binarySearchCoordinateBlockingPath size startPos endPos coordinates =
    let low = 0
    let high = (coordinates |> Seq.length) - 1

    Seq.unfold (fun (l, h) ->
        if l >= h then
            None
        else
            let m = (l + h) / 2
            let path =
                (coordinates |> Array.take (m + 1))
                |> gridFromCoordinates size size
                |> findShortestDistance startPos endPos
            match path with
            | Some _ -> Some (coordinates.[m + 1], (m + 1, h))
            | None -> Some (coordinates.[m + 1], (l, m))
    ) (low, high)
    |> Seq.last

let (blockingX, blockingY) = coordinates |> binarySearchCoordinateBlockingPath size startPos endPos

printfn "Coordinate blocking path: (%i,%i)" blockingX blockingY