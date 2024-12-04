open System.IO

let filePath = "04/input.txt"
let lines = File.ReadAllLines(filePath)

let grid =
    lines
    |> Array.map (fun row -> row.ToCharArray())
    |> array2D

let rows = Array2D.length1 grid
let cols = Array2D.length2 grid
let isInBounds r c = r >= 0 && r < rows && c >= 0 && c < cols

// Part 1
let part1 =
    let matchesXmasAtPositionInDirection row col direction =
        let word = "XMAS"
        let (rowStep, colStep) = direction

        let rec check index r c =
            if index = word.Length then true
            elif not (isInBounds r c) || grid.[r, c] <> word.[index] then false
            else check (index + 1) (r + rowStep) (c + colStep)
        
        check 0 row col

    let allInstances =
        let directions =
            [ 
                (0, 1)   // Right
                (0, -1)  // Left
                (1, 0)   // Down
                (-1, 0)  // Up
                (1, 1)   // Diagonal down-right
                (1, -1)  // Diagonal down-left
                (-1, 1)  // Diagonal up-right
                (-1, -1) // Diagonal up-left
            ]

        [ for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                for direction in directions do
                    if matchesXmasAtPositionInDirection row col direction then
                        yield (row, col, direction) ]

    let count = allInstances |> Seq.length
    printfn "Number of XMAS: %i" count

part1

// Part 2
let part2 =
    let matchesXPattern row col =
        let positions =
            [
                (row - 1, col - 1) // Top-left
                (row - 1, col + 1) // Top-right
                (row + 1, col - 1) // Bottom-left
                (row + 1, col + 1) // Bottom-right
                (row, col) // Center
            ]

        if positions |> List.forall (fun (r, c) -> isInBounds r c) then
            let chars = positions |> List.map (fun (r, c) -> grid.[r, c])
            chars = ['M'; 'M'; 'S'; 'S'; 'A'] || chars = ['M'; 'S'; 'M'; 'S'; 'A'] || chars = ['S'; 'M'; 'S'; 'M'; 'A'] || chars = ['S'; 'S'; 'M'; 'M'; 'A']
        else false

    let allInstances =
        [ for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                if matchesXPattern row col then
                    yield (row, col) ]

    let count = allInstances |> Seq.length
    printfn "Number of X-Pattern: %i" count

part2