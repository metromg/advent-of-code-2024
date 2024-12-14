open System
open System.IO
open System.Text.RegularExpressions

let filePath = "14/input.txt"
let input = File.ReadAllLines(filePath)

let robotRegex = Regex(@"p=(-*\d+),(-*\d+) v=(-*\d+),(-*\d+)")
let robots =
    input
    |> Array.map (fun robotInput ->
        let regexMatch = robotRegex.Match(robotInput)

        let posX = int regexMatch.Groups.[1].Value
        let posY = int regexMatch.Groups.[2].Value
        let velocityX = int regexMatch.Groups.[3].Value
        let velocityY = int regexMatch.Groups.[4].Value

        (posX, posY), (velocityX, velocityY)
    )
let width, height = 101, 103

let calculateEndPosition iterations width height (position, velocity) =
    let scale scalar (x, y) =
        (x * scalar, y * scalar)
    let add (ax, ay) (bx, by) =
        (ax + bx, ay + by)
    
    let deltaAfterIterations = velocity |> scale iterations
    let (newX, newY) = position |> add deltaAfterIterations

    let wrap max value =
        if value >= 0 then
            value % max
        else
            (max + (value % max)) % max

    (newX |> wrap width, newY |> wrap height)

let calculateEndPositionsAfterIterations iterations width height robots =
    robots |> Array.map (calculateEndPosition iterations width height)

// Part 1
type Quadrant = TopLeft | TopRight | BottomLeft | BottomRight

let calculateSafetyFactor width height positions =
    let quadrant pos =
        let maxX = width - 1
        let maxY = height - 1

        let between min max value =
            min <= value && value <= max
        let inFirstHalf max value =
            value |> between 0 (max / 2 - 1)
        let inSecondHalf max value =
            value |> between (max / 2 + 1) max

        match pos with
        | (x, y) when x |> inFirstHalf maxX && y |> inFirstHalf maxY -> Some TopLeft
        | (x, y) when x |> inSecondHalf maxX && y |> inFirstHalf maxY -> Some TopRight
        | (x, y) when x |> inFirstHalf maxX && y |> inSecondHalf maxY -> Some BottomLeft
        | (x, y) when x |> inSecondHalf maxX && y |> inSecondHalf maxY -> Some BottomRight
        | _ -> None
    
    positions
    |> Array.groupBy quadrant
    |> Array.map (fun (quadrant, quadrantPositions) ->
        match quadrant with
        | Some _ -> quadrantPositions |> Array.length
        | None -> 1
    )
    |> Array.fold (fun acc numberOfRobotsInQuadrant -> acc * numberOfRobotsInQuadrant) 1

let safetyFactorAfterIterations iterations width height robots =
    robots
    |> calculateEndPositionsAfterIterations iterations width height
    |> calculateSafetyFactor width height

let safetyFactorAfter100Iterations = safetyFactorAfterIterations 100 width height robots
printfn "Safety factor after 100 seconds: %i" safetyFactorAfter100Iterations

// Part 2

// We try to find a pattern which looks like a christmas tree
//
// 1) We assume that the tip of the christmas tree has to look something like this:
//
//      *   
//     ***  
//    *****
//
// 2) It is true that the robots were at every possible position after maximum width*height iterations.
//    The patterns repeat from there and we only need to check up to this point.
//    This is because by applying the modulo operation we limit the possible states of each robot in x and y direction by the width and height.
//    Every robot can be at every unique position inside the available space, and the number of unique positions in that space is width*height.
//    If we have more iterations than available states, one of the states must repeat.

let containsChristmasTreeTipPattern positions =
    let positionSet = positions |> Set.ofArray
    positionSet |> Set.exists (fun (x, y) ->
        let patternPositions = [
            (x, y) ;
            (x - 1, y + 1) ;
            (x, y + 1) ;
            (x + 1, y + 1) ;
            (x - 2, y + 2) ;
            (x - 1, y + 2) ;
            (x, y + 2) ;
            (x + 1, y + 2) ;
            (x + 2, y + 2)
        ]

        patternPositions |> List.forall (fun patternPos ->
            positionSet |> Set.contains patternPos
        )
    )

let lowestNumberOfIterationsForChristmasTreeTipPattern width height robots =
    let maxIterations = width * height
    [ for i in 1 .. maxIterations do
        let endPositions = robots |> calculateEndPositionsAfterIterations i width height
        if endPositions |> containsChristmasTreeTipPattern then
            yield i ]
    |> List.min

let lowestNumberOfIterationsForChristmasTree = lowestNumberOfIterationsForChristmasTreeTipPattern width height robots
printfn "Fewest number of seconds for christmas tree: %i" lowestNumberOfIterationsForChristmasTree

// Output as file
let outputFile width height positions =
    let text =
        [ for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                if positions |> Array.contains (x, y) then
                    yield "*"
                else
                    yield " "
            yield Environment.NewLine ]
        |> List.fold (fun acc value -> acc + value) ""
    
    File.WriteAllText("14/output.txt", text)

outputFile width height (calculateEndPositionsAfterIterations lowestNumberOfIterationsForChristmasTree width height robots)