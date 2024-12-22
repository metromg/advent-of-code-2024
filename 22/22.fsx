open System.IO

let filePath = "22/input.txt"
let input = File.ReadAllLines(filePath)

let inputNumbers = input |> Array.map int64

let mix a b =
    a ^^^ b

let prune a =
    a % 16777216L

let stage1 a =
    a * 64L |> mix a |> prune

let stage2 a =
    a / 32L |> mix a |> prune

let stage3 a =
    a * 2048L |> mix a |> prune

let calculate a =
    a |> stage1 |> stage2 |> stage3

// Part 1
let calculateNth n a =
    [0 .. n - 1] |> List.fold (fun next _ -> calculate next) a

let sumNth n numbers =
    numbers
    |> Seq.map (calculateNth n)
    |> Seq.sum

let sum2000th = inputNumbers |> sumNth 2000
printfn "Sum of the 2000th secret numbers: %i" sum2000th

// Part 2
let bananas a =
    a % 10L

let calculateMostBananas n numbers =
    numbers
    |> Seq.fold (fun sequenceToTotalBananasMap number ->
        [0 .. n - 1]
        |> List.fold (fun (currentMap, prevChanges, addedSeqs, secret) _ ->
            let newSecret = calculate secret

            let prevPrice = bananas secret
            let newPrice = bananas newSecret

            let change = newPrice - prevPrice
            let newChanges = change :: prevChanges

            let lastFourChanges = newChanges |> List.truncate 4
            if (lastFourChanges |> List.length) = 4 && not (addedSeqs |> Set.contains lastFourChanges) then
                let newMap = currentMap |> Map.change lastFourChanges (function Some v -> Some (v + newPrice) | None -> Some newPrice)
                let newVisitedSeqs = addedSeqs |> Set.add lastFourChanges
                (newMap, newChanges, newVisitedSeqs, newSecret)
            else
                (currentMap, newChanges, addedSeqs, newSecret)
        ) (sequenceToTotalBananasMap, [], Set.empty, number)
        |> (fun (m, _, _, _) -> m)
    ) Map.empty
    |> Map.toList
    |> List.map snd
    |> List.max

let mostBananas = inputNumbers |> calculateMostBananas 2000
printfn "Most possible bananas: %i" mostBananas