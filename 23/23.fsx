open System
open System.IO

let filePath = "23/input.txt"
let input = File.ReadAllLines(filePath)

let buildGraph input =
    input
    |> Seq.fold (fun graph (connection: string) ->
        let addConnection source target graph =
            graph |> Map.change source (function Some e -> Some (e |> Set.add target) | None -> Some (Set.empty |> Set.add target))

        let split = connection.Split('-')
        let nodeA = split.[0]
        let nodeB = split.[1]

        graph |> addConnection nodeA nodeB |> addConnection nodeB nodeA
    ) Map.empty

// Part 1
let findCliquesOfThree graph =
    graph
    |> Map.fold (fun cliques node adjacentNodes ->
        adjacentNodes
        |> Set.fold (fun cliques adjNode ->
            let foundCliques =
                graph
                |> Map.find adjNode
                |> Set.filter (fun n -> adjacentNodes |> Set.contains n)
                |> Set.map (fun n -> Set([node ; adjNode ; n]))
            cliques |> Set.union foundCliques
        ) cliques
    ) Set.empty

let filterCliquesByT (cliques: Set<Set<string>>) =
    cliques
    |> Set.filter (fun clique -> clique |> Set.exists (fun c -> c.StartsWith("t")))

let cliquesOfThreeWithT = input |> buildGraph |> findCliquesOfThree |> filterCliquesByT |> Set.count
printfn "Number of T-cliques of three: %i" cliquesOfThreeWithT

// Part 2
let findMaximalCliques graph =
    let neighbors node graph =
        graph |> Map.find node

    let rec find currentClique candidates cliques =
        if candidates |> Set.isEmpty then
            cliques |> Set.add currentClique
        else
            let pivot = candidates |> Set.minElement
            let pivotNeighbors = graph |> neighbors pivot

            Set.difference candidates pivotNeighbors
                |> Set.fold (fun acc node ->
                    let neighborNodes = graph |> neighbors node
                    find (currentClique |> Set.add node) (Set.intersect candidates neighborNodes) acc
                ) cliques

    find Set.empty (graph |> Map.keys |> Set.ofSeq) Set.empty

let findBiggestClique maximalCliques =
    maximalCliques
    |> Set.toList
    |> List.maxBy (fun clique -> clique |> Set.count)

let commaseparated (seq: string seq) =
    String.Join(',', seq)

let biggestClique = input |> buildGraph |> findMaximalCliques |> findBiggestClique |> commaseparated
printfn "Biggest clique: %s" biggestClique