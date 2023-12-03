open System
open System.IO

let lines = File.ReadAllLines("input.txt")

let allPartNumbers =
    let partNumbers x line =
        let folder (xs : (int * string) list) (p : int, v : string) =
            match xs with
            | [] -> [(p, v)]
            | (px, vx) :: t when px + 1 = p -> (p, vx + v) :: t
            | _ -> (p, v) :: xs

        line
        |> Seq.toList
        |> List.indexed
        |> List.filter (fun e -> Char.IsDigit(snd e))
        |> List.map (fun e -> (fst e, string (snd e)))
        |> List.fold folder [] // first element is index of least significant digit
        |> List.map (fun e -> ((x, (fst e) - (snd e).Length + 1), (snd e)))

    lines
    |> Array.toList
    |> List.mapi partNumbers
    |> List.collect (fun e -> e)

let allNeighbors ((x : int, y : int), s : string) =
    [for i in y - 1..y + s.Length -> (x - 1, i)] @
    [(x, y + s.Length)] @
    [for i in y - 1..y + s.Length -> (x + 1, i)] @
    [(x, y - 1)]

let onMap (map : char array2d) (x : int, y : int) =
    x >= 0 && x < (Array2D.length1 map) && y >= 0 && y < (Array2D.length2 map)

let isSymbol (map : char array2d) (x : int, y : int) = not (Char.IsNumber map[x, y]) && map[x, y] <> '.'

let isGear (map : char array2d) (x : int, y : int) = map[x, y] = '*'

let map = lines |> array2D

let part1 =
    allPartNumbers
    |> List.where (fun p ->
        allNeighbors p
        |> List.filter (onMap map)
        |> List.exists (isSymbol map))
    |> List.map (snd >> int)
    |> List.sum
printfn "Answer part 1 %i" part1

let part2 =
    allPartNumbers
    |> List.collect (fun p ->
        allNeighbors p
        |> List.where (onMap map)
        |> List.where (isGear map)
        |> List.map (fun g -> (g, p)))
    |> List.groupBy fst
    |> List.where (fun e -> List.length (snd e) = 2)
    |> List.map (snd >> (List.map (snd >> snd >> int) >> List.reduce (*)))
    |> List.sum
printfn "Answer part 2: %i" part2