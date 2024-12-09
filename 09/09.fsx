open System.IO

let filePath = "09/input.txt"
let input = File.ReadAllText(filePath)

// Part 1
type Block =
    | FileBlock of int
    | FreeSpaceBlock

let isFileBlock block =
    match block with
    | FileBlock _ -> true
    | _ -> false
let isFreeSpaceBlock block =
    match block with
    | FreeSpaceBlock -> true
    | _ -> false

let blocks =
    input.ToCharArray()
    |> Array.map (fun char -> int (char.ToString()))
    |> Array.mapi (fun i length ->
        if i % 2 = 0 then
            let fileIdentifier = i / 2
            Array.init length (fun _ -> FileBlock(fileIdentifier))
        else
            Array.init length (fun _ -> FreeSpaceBlock)
    )
    |> Array.concat

let swapLastFileBlockWithFirstFreeSpace blocks =
    let swap index1 index2 values =
        values
        |> Array.permute (fun i ->
            if i = index1 then index2
            elif i = index2 then index1
            else i
        )

    let freeSpaceIndex = blocks |> Array.findIndex isFreeSpaceBlock
    let fileBlockIndex = blocks |> Array.findIndexBack isFileBlock
    
    if freeSpaceIndex < fileBlockIndex then
        Some (blocks |> swap freeSpaceIndex fileBlockIndex)
    else
        None

let checksum1 =
    Seq.unfold (fun currentBlocks ->
        let newBlocks = swapLastFileBlockWithFirstFreeSpace currentBlocks

        match newBlocks with
        | Some b -> Some (b, b)
        | None -> None
    ) blocks
    |> Seq.last
    |> Array.mapi (fun i block ->
        match block with
        | FileBlock id -> (int64 i) * (int64 id)
        | FreeSpaceBlock -> 0
    )
    |> Array.sum

printfn "Checksum part 1: %i" checksum1

// Part 2
type Segment =
    | FileSegment of int * int
    | FreeSpaceSegment of int

let segments =
    input.ToCharArray()
    |> Array.map (fun char -> int (char.ToString()))
    |> Array.mapi (fun i length ->
        if i % 2 = 0 then
            let fileIdentifier = i / 2
            FileSegment(length, fileIdentifier)
        else
            FreeSpaceSegment(length)
    )

let fitFileSegmentByIdentifierIntoFirstFittingFreeSpace identifier segments =
    let (fileSegmentIndex, fileSegment) =
        segments
        |> Array.indexed
        |> Array.findBack (fun (_, s) ->
            match s with
            | FileSegment (_, id) -> id = identifier
            | FreeSpaceSegment _ -> false
        )
    let fileSegmentLength =
        match fileSegment with
        | FileSegment (length, _) -> length
        | _ -> 0

    let (freeSpaceIndex, freeSpaceSegment) =
        segments
        |> Array.indexed
        |> Array.find (fun (_, s) ->
            match s with
            | FreeSpaceSegment freeSpaceLength -> freeSpaceLength >= fileSegmentLength
            | _ -> false
        )
    let freeSpaceSegmentLength =
        match freeSpaceSegment with
        | FreeSpaceSegment length -> length
        | _ -> 0

    if fileSegmentIndex > freeSpaceIndex then
        let segmentList = segments |> Array.toList
        let updatedSegments =
            (segmentList |> List.take freeSpaceIndex)
            @ [fileSegment ; FreeSpaceSegment(freeSpaceSegmentLength - fileSegmentLength)] 
            @ (segmentList |> List.take fileSegmentIndex |> List.skip (freeSpaceIndex + 1)) 
            @ [FreeSpaceSegment(fileSegmentLength)] 
            @ (segmentList |> List.skip (fileSegmentIndex + 1))
        updatedSegments |> List.toArray
    else
        segments

let checksum2 =
    Seq.unfold (fun (currentSegments, currentFileIdentifier) ->
        let newSegments = fitFileSegmentByIdentifierIntoFirstFittingFreeSpace currentFileIdentifier currentSegments

        if currentFileIdentifier > 0 then
            Some (newSegments, (newSegments, currentFileIdentifier - 1))
        else
            None
    ) (segments, segments.Length / 2)
    |> Seq.last
    |> Array.map (fun segment ->
        match segment with
        | FileSegment (length, identifier) -> Array.init length (fun _ -> FileBlock(identifier))
        | FreeSpaceSegment length -> Array.init length (fun _ -> FreeSpaceBlock)
    )
    |> Array.concat
    |> Array.mapi (fun i block ->
        match block with
        | FileBlock id -> (int64 i) * (int64 id)
        | FreeSpaceBlock -> 0
    )
    |> Array.sum

printfn "Checksum part 2: %i" checksum2