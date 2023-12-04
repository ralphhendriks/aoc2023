open System
open System.IO

let lines = File.ReadAllLines("input.txt")

let scratchcardsInput =
    lines
    |> Array.map (fun line ->
        let parts = line.Split([| ':'; '|' |])
        let cardNumber = int (parts[0].Substring(4).Trim())
        let winningNumbers = parts[1].Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Set.ofArray
        let numbersYouHave = parts[2].Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int |> Set.ofArray
        (cardNumber, winningNumbers, numbersYouHave))

let winningNumbers =
    scratchcardsInput
    |> Array.map (fun (c, nyh, wn) -> (c, Set.intersect nyh wn |> Set.count))

let part1 =
    winningNumbers
    |> Array.map (fun (_, n) -> pown 2 (n - 1))
    |> Array.sum
printfn "Answer part 1: %i" part1

let addCopies (cardPile : Map<int, (int * int)>) copies =
    let addSingle cardPile (cardNumber : int, numberOfCopies : int) =
        match Map.tryFind cardNumber cardPile with
        | None -> cardPile
        | Some (n, w) -> Map.add cardNumber (n + numberOfCopies, w) cardPile
    copies |> Seq.fold addSingle cardPile

let copyAlg (cardPile : Map<int, (int * int)>) (cardNumber : int) =
    let numberOfCopies = fst cardPile[cardNumber]
    let winningNumber = snd cardPile[cardNumber]
    if winningNumber = 0 then cardPile
    else
        let s = Seq.map (fun i -> (i, numberOfCopies)) { cardNumber + 1 .. cardNumber + winningNumber }
        addCopies cardPile s

// cardNumber -> (numberOfCopies, winningNumber), initially all scratchcards have 1 copy
let initialCardPile = winningNumbers |> Array.map (fun (n, w) -> (n, (1, w))) |> Map.ofArray
let finalCardPile = initialCardPile |> Map.keys |> Seq.sort |> Seq.fold copyAlg initialCardPile
let part2 = finalCardPile |> Map.values |> Seq.map fst |> Seq.sum
printfn "Answer part 2: %i" part2