open System.IO

let lines = File.ReadAllLines("input.txt")

let parseSet (s:string) =
    let parseNumberAndColor (s:string) =
        let elems = s.Split(' ')
        match elems[1] with
        | "red" -> (int elems[0], 0, 0)
        | "green" -> (0, int elems[0], 0)
        | "blue" -> (0, 0, int elems[0])
        | _ -> failwith "Unrecognized color."

    let addPairs (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

    s.Split(", ")
    |> Array.map parseNumberAndColor
    |> Array.reduce addPairs

let games =
    lines
    |> Array.map (fun s ->
        let a = s.Split(": ")
        let gameId = int (a[0].Substring(5))
        let sets = a[1].Split("; ") |> Array.map parseSet
        (gameId, sets))

let maxPairs (r1, g1, b1) (r2, g2, b2) = (max r1 r2, max g1 g2, max b1 b2)

let isImpossible (r1, g1, b1) (r2, g2, b2) = r2 > r1 || g2 > g1 || b2 > b1

let power (r, g, b) = r * g * b

let part1 =
    games
    |> Array.map (fun e ->
        let z =
            snd e
            |> Array.reduce maxPairs
        (fst e, z))
    |> Array.filter (fun e -> not (isImpossible (12, 13, 14) (snd e)))
    |> Array.sumBy fst
printfn "Answer part 1: %i" part1

let part2 =
    games
    |> Array.map (snd >> Array.reduce maxPairs >> power)
    |> Array.sum
printfn "Answer part 2: %i" part2