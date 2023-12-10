open System.IO

type Direction = Left | Right

let lines = File.ReadAllLines("input.txt") |> Array.toList

let parseDirection = function | 'L' -> Left | 'R' -> Right | _ -> failwith "invalid character"

let directions = Seq.map parseDirection lines[0] |> Seq.toList

let map =
    lines
    |> List.skip 2
    |> List.map (fun l ->
        let node = l.Substring(0, 3)
        let left = l.Substring(7, 3)
        let right = l.Substring(12, 3)
        (node, (left, right)))
    |> Map.ofList

let fold (num, current) dir =
    (
        num + 1L,
       (match dir with | Left -> fst | Right -> snd) map[current]
    )

let part1 =
    seq { while true do yield! directions }
    |> Seq.scan fold (0, "AAA")
    |> Seq.skipWhile (fun (_, node) -> node <> "ZZZ")
    |> Seq.head
    |> fst

printfn "Answer part 1: %i" part1

// The solution below is not generic. It works because the following two conditions hold true:
// 1. The amount of steps needed to get from a start node (`..A`) to the first end node (`..Z`) are equal to the amount
//    of steps needed to get from the end node to the end node again.
// 2. For all paths through the puzzle, once in an end node (`..Z`) you always end up in that same end node. That is,
//    you do not visit other end nodes in a cycle.
// This is clever puzzle design, but also feels a bit too fabricated. A generalized solution would be much harder.

let lcm x y =
    let rec gcd x y =
        if y = 0L then x
        else gcd y (x % y)
    x * y / (gcd x y)

let initialNodes = map |> Map.keys |> Seq.where (fun s -> s[2] = 'A') |> Seq.toList

let part2 =
    initialNodes
    |> List.map (fun initialNode ->
        seq { while true do yield! directions }
        |> Seq.scan fold (0L, initialNode)
        |> Seq.skipWhile (fun (_, node) -> node[2] <> 'Z')
        |> Seq.head
        |> fst)
    |> List.reduce lcm

printfn "Answer part 2: %i" part2