open System
open System.IO
open System.Numerics

let filePath = "11/input.txt"
let input = File.ReadAllText(filePath)

let stoneToCountMap =
    input.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun stone -> BigInteger.Parse(stone), BigInteger.One)
    |> Map.ofArray

let addToCountOfStone stone count =
    Map.change stone (function Some v -> Some(v + count) | None -> Some count)

let blink stoneToCountMap =
    stoneToCountMap
    |> Map.fold (fun acc stone count ->
        let stoneAsString = stone.ToString()
        if stone = BigInteger.Zero then
            acc |> addToCountOfStone BigInteger.One count
        elif stoneAsString.Length % 2 = 0 then
            let middleIndex = stoneAsString.Length / 2
            let left = BigInteger.Parse(stoneAsString.[..middleIndex - 1])
            let right = BigInteger.Parse(stoneAsString.[middleIndex..])
            acc
            |> addToCountOfStone left count
            |> addToCountOfStone right count
        else
            let value = stone * BigInteger(2024)
            acc |> addToCountOfStone value count
    ) Map.empty

let applyBlinks blinks stoneToCountMap =
    Seq.init blinks id
    |> Seq.fold (fun currentMap _ -> blink currentMap) stoneToCountMap

let totalCount =
    Map.fold (fun acc _ count -> acc + count) BigInteger.Zero

// Part 1
stoneToCountMap
|> applyBlinks 25
|> totalCount
|> printfn "Number of stones after 25 times: %A"

// Part 2
stoneToCountMap
|> applyBlinks 75
|> totalCount
|> printfn "Number of stones after 75 times: %A"