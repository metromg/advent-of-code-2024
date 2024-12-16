open System.IO

let filePath = "16/input.txt"
let input = File.ReadAllLines(filePath)

type Direction = East | South | West | North

let buildGraph (input: string array) =
    let positionValues =
        input
        |> Array.mapi (fun rowIndex row ->
            row.ToCharArray() |> Array.mapi (fun colIndex value ->
                (colIndex, rowIndex, value)
            )
        )
        |> Array.concat

    let nodes =
        positionValues
        |> Array.collect (fun (x, y, value) ->
            match value with
            | '.' | 'S' | 'E' -> [| East ; South ; West ; North |] |> Array.map (fun dir -> (x, y, dir))
            | _ -> [||]
        )

    let findNode (x, y, dir) nodes =
        nodes |> Array.tryFind (fun (nx, ny, nDir) -> nx = x && ny = y && nDir = dir)

    let findNeighbor (x, y, dir) nodes =
        match dir with
        | East -> nodes |> findNode (x + 1, y, East)
        | South -> nodes |> findNode (x, y + 1, South)
        | West -> nodes |> findNode (x - 1, y, West)
        | North -> nodes |> findNode (x, y - 1, North)
        
    let graph =
        nodes
        |> Array.map (fun (x, y, dir) ->
            let weightedEdgeToNeighbor = (nodes |> findNeighbor (x, y, dir), 1)
            let weightedEdgeToRotation =
                match dir with
                | East | West -> [ North ; South ]
                | North | South -> [ East ; West ]
                |> List.map (fun direction -> ((x, y, direction), 1000))
            
            let weightedEdges =
                match weightedEdgeToNeighbor with
                | (Some node, weight) -> (node, weight) :: weightedEdgeToRotation
                | _ -> weightedEdgeToRotation
            
            (x, y, dir), weightedEdges
        )
        |> Map.ofArray

    let (startX, startY, _) = positionValues |> Array.find (fun (_, _, value) -> value = 'S')
    let (endX, endY, _) = positionValues |> Array.find (fun (_, _, value) -> value = 'E')

    graph, (startX, startY), (endX, endY)

let dijkstra startNode graph =
    let allNodes = graph |> Map.keys |> Seq.toList
    let initialPredecessors = 
        allNodes |> List.fold (fun acc node -> acc |> Map.add node []) Map.empty
    
    Seq.unfold (fun (priorityQueue, distances, visitedNodes, predecessors) ->
        if priorityQueue |> Set.isEmpty then
            None
        else
            let (currentNodeDistance, currentNode) = priorityQueue |> Set.minElement
            let poppedPriorityQueue = priorityQueue |> Set.remove (currentNodeDistance, currentNode)
            if visitedNodes |> Set.contains currentNode then
                Some ((distances, predecessors), (poppedPriorityQueue, distances, visitedNodes, predecessors))
            else
                let newVisitedNodes = visitedNodes |> Set.add currentNode
                let (newPriorityQueue, newDistances, newPredecessors) =
                    graph
                    |> Map.find currentNode
                    |> List.fold (fun (currentPriorityQueue, currentDistances, currentPredecessors) (connectedNode, weight) ->
                        if newVisitedNodes |> Set.contains connectedNode then
                            (currentPriorityQueue, currentDistances, currentPredecessors)
                        else
                            let newDistance = currentNodeDistance + weight
                            let currentConnectedNodeDistance = currentDistances |> Map.tryFind connectedNode

                            let updateDistanceAndSetPredecessorWhenShorterDistance () =
                                let updatedPriorityQueue = currentPriorityQueue |> Set.add (newDistance, connectedNode)
                                let updatedDistances = currentDistances |> Map.add connectedNode newDistance
                                let updatedPredecessors = currentPredecessors |> Map.add connectedNode [currentNode]
                                (updatedPriorityQueue, updatedDistances, updatedPredecessors)

                            let updatePredecessorsWhenEqualDistance () =
                                let updatedPredecessors =
                                    currentPredecessors |> Map.add connectedNode (currentNode :: (currentPredecessors |> Map.find connectedNode))
                                (currentPriorityQueue, currentDistances, updatedPredecessors)

                            let skipWhenLongerDistance () =
                                (currentPriorityQueue, currentDistances, currentPredecessors)

                            match currentConnectedNodeDistance with
                            | Some distance when newDistance < distance -> updateDistanceAndSetPredecessorWhenShorterDistance ()
                            | Some distance when newDistance = distance -> updatePredecessorsWhenEqualDistance ()
                            | None -> updateDistanceAndSetPredecessorWhenShorterDistance ()
                            | _ -> skipWhenLongerDistance ()
                    ) (poppedPriorityQueue, distances, predecessors)
                Some ((newDistances, newPredecessors), (newPriorityQueue, newDistances, newVisitedNodes, newPredecessors))
    ) (Set([ (0, startNode) ]), Map([ (startNode, 0) ]), Set.empty, initialPredecessors)
    |> Seq.last

let graph, (startX, startY), (endX, endY) = buildGraph input
let (distances, predecessors) = dijkstra (startX, startY, East) graph

// Part 1
let findLowestDistance (x, y) distances =
    [ East ; South ; West ; North ]
    |> List.map (fun dir -> (x, y, dir))
    |> List.map (fun node -> distances |> Map.find node)
    |> List.min

let lowestDistanceToEndPos = findLowestDistance (endX, endY) distances

printfn "Lowest distance to end position: %i" lowestDistanceToEndPos

// Part 2
let backtrack predecessors endNode =
    let rec collect contributingNodes stack =
        match stack with
        | [] -> contributingNodes
        | currentNode :: rest ->
            if contributingNodes |> Set.contains currentNode then
                collect contributingNodes rest
            else
                let newNodes = predecessors |> Map.find currentNode
                collect (contributingNodes |> Set.add currentNode) (newNodes @ rest)
    collect Set.empty [endNode]

let numberOfNodesOnShortestPaths =
    backtrack predecessors (endX, endY, North)
    |> Set.map (fun (x, y, _) -> (x, y))
    |> Set.count

printfn "Number of nodes on shortest paths: %i" numberOfNodesOnShortestPaths