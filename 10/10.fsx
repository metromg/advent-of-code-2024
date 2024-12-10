open System.IO

let filePath = "10/input.txt"
let input = File.ReadAllLines(filePath)

let grid =
    input
    |> Array.map (fun line -> line.ToCharArray() |> Array.map (string >> int))
    |> array2D

type Direction = Up | Right | Down | Left

let move dir (y, x) =
    match dir with
    | Up -> (y - 1, x)
    | Right -> (y, x + 1)
    | Down -> (y + 1, x)
    | Left -> (y, x - 1)

let isInBounds grid (y, x) =
    0 <= y && y < Array2D.length1 grid && 0 <= x && x < Array2D.length2 grid

let trailHeads grid =
    [ for y in 0 .. Array2D.length1 grid - 1 do
        for x in 0 .. Array2D.length2 grid - 1 do
            if grid.[y, x] = 0 then
                yield (y, x) ]

type Mode = Score | Rating

let exploreTrails mode grid trailHeadPos =
    let rec explore position currentHeight =
        if currentHeight = 9 then
            [ Some position ]
        else
            let nextHeight = currentHeight + 1
            [ Up ; Right ; Down ; Left ]
            |> List.collect (fun dir ->
                let nextPos = move dir position
                if isInBounds grid nextPos && grid.[fst nextPos, snd nextPos] = nextHeight then
                    explore nextPos nextHeight
                else
                    [ None ]
            )
            |> List.filter Option.isSome

    explore trailHeadPos 0
    |> (if mode = Score then List.distinct else id)
    |> List.length

// Part 1
let sumOfTrailHeadScores =
    grid
    |> trailHeads
    |> List.map (exploreTrails Score grid)
    |> List.sum

printfn "Sum of trail head scores: %i" sumOfTrailHeadScores

// Part 2
let sumOfTrailHeadRatings =
    grid
    |> trailHeads
    |> List.map (exploreTrails Rating grid)
    |> List.sum

printfn "Sum of trail head ratings: %i" sumOfTrailHeadRatings