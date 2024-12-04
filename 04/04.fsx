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

    let matchesAtPositionInDirection (grid: char array2d) row col direction (word: string) =
        let (rowStep, colStep) = direction
        let wordLength = word.Length

        let rec check index r c =
            if index = wordLength then true
            elif not (isInBounds r c) || grid.[r, c] <> word.[index] then false
            else check (index + 1) (r + rowStep) (c + colStep)
        
        check 0 row col

    let findAllInstances grid word =
        let rows = Array2D.length1 grid
        let cols = Array2D.length2 grid

        [ for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                for direction in directions do
                    if matchesAtPositionInDirection grid row col direction word then
                        yield (row, col, direction) ]

    let count = findAllInstances grid "XMAS" |> Seq.length
    printfn "Number of XMAS: %i" count

part1

// Part 2
let part2 =
    let matchesXPattern (grid: char array2d) row col =
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

    let findAllInstances grid =
        let rows = Array2D.length1 grid
        let cols = Array2D.length2 grid
        
        [ for row in 0 .. rows - 1 do
            for col in 0 .. cols - 1 do
                if matchesXPattern grid row col then
                    yield (row, col) ]

    let count = findAllInstances grid |> Seq.length
    printfn "Number of X-Pattern: %i" count

part2