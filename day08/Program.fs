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