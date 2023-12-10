open System.IO

let inputLines = File.ReadAllLines("input.txt") |> Array.toList

let parseLine (s: string) = s.Split(" ") |> Array.toList |> List.map int

let rec extrapolate (history: int list) =
    match (List.forall ((=) 0) history) with
    | true -> 0
    | false ->
        let delta = history |> List.pairwise |> List.map (fun (x, y) -> y - x) |> extrapolate
        delta + List.last history

let part1 = inputLines |> List.map (parseLine >> extrapolate) |> List.sum
printfn "Answer part 1: %i" part1

let rec extrapolateBack (history: int list) =
    match (List.forall ((=) 0) history) with
    | true -> 0
    | false ->
        let delta = history |> List.pairwise |> List.map (fun (x, y) -> y - x) |> extrapolateBack
        (List.head history) - delta

let part2 = inputLines |> List.map (parseLine >> extrapolateBack) |> List.sum
printfn "Answer part 2: %i" part2