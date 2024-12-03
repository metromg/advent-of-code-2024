open System
open System.IO

let readReportsFromFile filePath =
    let lines = File.ReadAllLines(filePath)
    lines |> Array.map (fun line -> line.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

let filePath = "02/input.txt"
let reports = readReportsFromFile filePath

// Part 1
let isSafeReport levels =
    let differences =
        levels
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> b - a)

    let allIncreasingAndSafe = differences |> Seq.forall (fun diff -> diff > 0 && diff <= 3)
    let allDecreasingAndSafe = differences |> Seq.forall (fun diff -> diff < 0 && diff >= -3)

    allIncreasingAndSafe || allDecreasingAndSafe

let countSafeReports reports =
    reports
    |> Array.filter isSafeReport
    |> Array.length

let safeReportsCount = countSafeReports reports

printfn "Number of safe reports: %i" safeReportsCount

// Part 2
let isSafeReportAfterRemovalOfSingleLevel levels =
    let removeNth n list =
        let part1 = Seq.take n list
        let part2 = Seq.skip (n + 1) list
        Seq.append part1 part2

    levels
    |> Seq.mapi (fun n _ -> removeNth n levels)
    |> Seq.exists isSafeReport

let countSafeReportsWithRemovalOfSingleLevel reports =
    reports
    |> Array.filter (fun levels -> isSafeReport levels || isSafeReportAfterRemovalOfSingleLevel levels)
    |> Array.length

let safeReportsWithRemovalOfSingleLevelCount = countSafeReportsWithRemovalOfSingleLevel reports

printfn "Number of safe reports with removal of single level: %i" safeReportsWithRemovalOfSingleLevelCount