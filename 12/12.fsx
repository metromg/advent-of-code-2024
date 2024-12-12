open System.IO

let filePath = "12/input.txt"
let input = File.ReadAllLines(filePath)

let grid =
    input
    |> Array.map (fun line -> line.ToCharArray())
    |> array2D

let isInBounds grid (y, x) =
    0 <= y && y < Array2D.length1 grid && 0 <= x && x < Array2D.length2 grid

type Direction = Top | Bottom | Left | Right

let neighbor (y, x) dir =
    match dir with
    | Top -> (y - 1, x)
    | Bottom -> (y + 1, x)
    | Left -> (y, x - 1)
    | Right -> (y, x + 1)

let neighbors pos =
    [ Top ; Bottom ; Left ; Right ]
    |> List.map (neighbor pos)

let exploreRegion grid start =
    let rec explore queue region =
        match queue with
        | [] -> region
        | pos :: rest when region |> Set.contains pos || not (pos |> isInBounds grid) || grid.[fst pos, snd pos] <> grid.[fst start, snd start] ->
            explore rest region
        | pos :: rest ->
            let newRegion = region |> Set.add pos
            let newQueue = neighbors pos @ rest
            explore newQueue newRegion

    explore [start] Set.empty

let groupRegions grid =
    let allCoordinates =
        [ for y in 0 .. Array2D.length1 grid - 1 do
            for x in 0 .. Array2D.length2 grid - 1 -> (y, x) ]
    
    allCoordinates
    |> List.fold (fun (regions, visited) pos ->
        if visited |> Set.contains pos then
            (regions, visited)
        else
            let region = exploreRegion grid pos
            (region :: regions, Set.union visited region)
    ) ([], Set.empty)
    |> fst

let calculateArea gardenPlotsInRegion =
    gardenPlotsInRegion |> Set.count

// Part 1
let calculatePerimeter gardenPlotsInRegion =
    gardenPlotsInRegion
    |> Seq.sumBy (fun pos ->
        neighbors pos
        |> List.filter (fun neighbor -> not (gardenPlotsInRegion |> Set.contains neighbor))
        |> List.length
    )

let calculateTotalPrice grid =
    grid
    |> groupRegions
    |> List.sumBy (fun region ->
        let area = calculateArea region
        let perimeter = calculatePerimeter region
        area * perimeter
    )

let totalPrice = grid |> calculateTotalPrice
printfn "Total price: %i" totalPrice

// Part 2
let calculateStraightSides gardenPlotsInRegion =
    let boundaryPositions dir =
        gardenPlotsInRegion
        |> Set.filter (fun pos ->
            not (gardenPlotsInRegion |> Set.contains (neighbor pos dir))
        )

    let countSegments positions isHorizontal =
        positions
        |> Seq.sortBy (if isHorizontal then snd else fst)
        |> Seq.groupBy (if isHorizontal then fst else snd)
        |> Seq.sumBy (fun (_, group) ->
            let gaps =
                group
                |> Seq.map (if isHorizontal then snd else fst)
                |> Seq.pairwise
                |> Seq.filter (fun (a, b) -> b - a > 1)
                |> Seq.length
            gaps + 1
        )

    [ Top ; Bottom ; Left ; Right ]
    |> List.sumBy (fun dir ->
        let boundaryPositionsInDir = boundaryPositions dir
        let isHorizontal = dir = Top || dir = Bottom

        countSegments boundaryPositionsInDir isHorizontal
    )

let calculateTotalPriceWithDiscount grid =
    grid
    |> groupRegions
    |> List.sumBy (fun region ->
        let area = calculateArea region
        let straightSides = calculateStraightSides region
        area * straightSides
    )

let totalPriceWithDiscount = grid |> calculateTotalPriceWithDiscount
printfn "Total price with discount: %i" totalPriceWithDiscount