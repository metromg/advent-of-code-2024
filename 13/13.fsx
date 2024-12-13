open System
open System.IO
open System.Numerics
open System.Text.RegularExpressions

let filePath = "13/input.txt"
let input = File.ReadAllText(filePath)

type ClawMachine =
    { AX: BigInteger
      AY: BigInteger
      BX: BigInteger
      BY: BigInteger
      X: BigInteger
      Y: BigInteger }

let inputRegex = Regex(@"Button A: X\+(\d+), Y\+(\d+)\s*Button B: X\+(\d+), Y\+(\d+)\s*Prize: X=(\d+), Y=(\d+)")
let clawMachines =
    input.Split([|Environment.NewLine + Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun clawMachineInput ->
        let regexMatch = inputRegex.Match(clawMachineInput)

        let ax = BigInteger.Parse(regexMatch.Groups.[1].Value)
        let ay = BigInteger.Parse(regexMatch.Groups.[2].Value)
        let bx = BigInteger.Parse(regexMatch.Groups.[3].Value)
        let by = BigInteger.Parse(regexMatch.Groups.[4].Value)
        let x = BigInteger.Parse(regexMatch.Groups.[5].Value)
        let y = BigInteger.Parse(regexMatch.Groups.[6].Value)

        { AX = ax ; AY = ay ; BX = bx ; BY = by ; X = x ; Y = y }
    )

let cost a b = BigInteger(3) * a + b

let findMinimalCost clawMachine =
    let ax, ay, bx, by, x, y =
        clawMachine.AX, clawMachine.AY, clawMachine.BX, clawMachine.BY, clawMachine.X, clawMachine.Y

    // system of linear equations:
    // x = a * ax + b * bx
    // y = a * ay + b * by
    //
    // using matrix notation:
    // | ax bx | | a |   | X |
    // |       | |   | = |   |
    // | ay by | | b |   | Y |
    // 
    // rearrange for a and b by inverting the coefficient matrix:
    // | a |   | ax bx |-1 | X |
    // |   | = |       |   |   |
    // | b |   | ay by |   | Y |
    let a = decimal (by * x - bx * y) / decimal (ax * by - bx * ay)
    let b = decimal (x - BigInteger(a) * ax) / decimal bx

    if a % 1m = 0m && b % 1m = 0m then
        cost (BigInteger(a)) (BigInteger(b))
    else
        BigInteger.Zero

let minimalCostToWinAllPossiblePrizes1 =
    clawMachines
    |> Array.map findMinimalCost
    |> Array.sum

printfn "Minimal cost to win all possible prizes (Part 1): %A" minimalCostToWinAllPossiblePrizes1

let minimalCostToWinAllPossiblePrizes2 =
    clawMachines
    |> Array.map (fun cm -> { cm with X = cm.X + BigInteger(10000000000000L) ; Y = cm.Y + BigInteger(10000000000000L) } )
    |> Array.map findMinimalCost
    |> Array.sum

printfn "Minimal cost to win all possible prizes (Part 2): %A" minimalCostToWinAllPossiblePrizes2