open System
open System.IO

let inputLines = File.ReadAllLines("input.txt")

let parseInputPart1 (lines : string[]) =
    let raceDurations =
        lines[0].Substring(5).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
    let recordDistances =
        lines[1].Substring(9).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
    Array.zip raceDurations recordDistances

let waysToWin (duration, record) =
    [0 .. duration]
    |> List.where (fun t -> (t * (duration - t)) > record)
    |> List.length

let part1 = parseInputPart1 inputLines |> Array.map waysToWin |> Array.reduce (*)
printfn "Answer part 1: %i" part1

let parseInputPart2 (lines : string []) =
    let raceDurations = lines[0].Substring(5).Replace(" ", "") |> int64
    let recordDistances = lines[1].Substring(9).Replace(" ", "") |> int64
    (raceDurations, recordDistances)

let waysToWin2 (duration : int64, record : int64) =
    let first = { 0L .. duration } |> Seq.find (fun t -> (t * (duration - t)) > record)
    let last =  { duration .. -1L .. 0L } |> Seq.find (fun t -> (t * (duration - t)) > record)
    last - first + 1L

let part2 = parseInputPart2 inputLines |> waysToWin2
printfn "Answer part 2: %i" part2