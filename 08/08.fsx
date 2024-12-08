open System.IO

let filePath = "08/input.txt"
let input = File.ReadAllLines(filePath)

type Position = (int * int)
type Antenna = Position * char

module Position =
    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
    let negate (x, y) = (-x, -y)
    let subtract a b = add a (negate b)
    let isInBounds maxX maxY (x, y) = 0 <= x && x <= maxX && 0 <= y && y <= maxY

module Antenna =
    let position (antenna: Antenna) = fst antenna
    let frequency (antenna: Antenna) = snd antenna

let antennas: Antenna array =
    input
    |> Array.mapi (fun rowIndex row ->
        row.ToCharArray()
        |> Array.mapi (fun colIndex value -> (colIndex, rowIndex), value)
        |> Array.filter (fun (_, value) -> value <> '.')
    )
    |> Array.concat

let maxX, maxY = input.[0].Length - 1, input.Length - 1

let antennaPairs antennas =
    antennas
    |> Seq.mapi (fun i a -> antennas |> Seq.skip (i + 1) |> Seq.map (fun b -> (a, b)))
    |> Seq.concat

let antinodeDirections a b =
    let dirFromA = Position.subtract a b
    let dirFromB = Position.subtract b a
    (dirFromA, dirFromB)

let findUniqueAntinodesInBoundsForGroup maxX maxY onePerDirection group =
    let rec explore position direction =
        if position |> Position.isInBounds maxX maxY then
            let nextPos = Position.add position direction
            position :: (explore nextPos direction)
        else
            List.empty

    group
    |> antennaPairs
    |> Seq.collect (fun (a, b) ->
        let antennaPosA, antennaPosB = Antenna.position a, Antenna.position b
        let dirFromA, dirFromB = antinodeDirections antennaPosA antennaPosB

        if onePerDirection then
            [Position.add antennaPosA dirFromA; Position.add antennaPosB dirFromB]
        else
            (explore antennaPosA dirFromA) @ (explore antennaPosB dirFromB)
    )
    |> Seq.distinct
    |> Seq.filter (Position.isInBounds maxX maxY)

let findUniqueAntinodesInBounds antennas maxX maxY onePerDirection =
    antennas
    |> Seq.groupBy Antenna.frequency
    |> Seq.collect (fun (_, group) -> group |> findUniqueAntinodesInBoundsForGroup maxX maxY onePerDirection)
    |> Seq.distinct

// Part 1
let part1 = findUniqueAntinodesInBounds antennas maxX maxY true |> Seq.length
printfn "Unique antinode positions in bounds with one per direction: %i" part1

// Part 2
let part2 = findUniqueAntinodesInBounds antennas maxX maxY false |> Seq.length
printfn "Unique antinode positions in bounds: %i" part2