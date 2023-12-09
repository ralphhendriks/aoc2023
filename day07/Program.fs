open System.IO

let lines = File.ReadAllLines("input.txt") |> Array.toList

type Card  = Ace | King | Queen |Jack |Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | Joker

type HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPair | OnePair | HighCard

let parseCard = function
| 'A' -> Ace | 'K' -> King | 'Q' -> Queen | 'J' -> Jack | 'T' -> Ten | '9' -> Nine | '8' -> Eight | '7' -> Seven | '6' -> Six | '5' -> Five | '4' -> Four | '3' -> Three | '2' -> Two
| _ -> failwith "Invalid card identifier"

let parseHand (s : string) = s |> Seq.map parseCard |> Seq.toList

let calculateHandType (hand : list<Card>) =
    let cardHistogram = List.countBy id hand
    let numberOfJokers = cardHistogram |> List.where (fun (c, _) -> c = Joker) |> List.sumBy snd
    let histogramWithoutJokers = cardHistogram |> List.where (fun (c, _) -> c <> Joker)
    let countsWithoutJokers = histogramWithoutJokers |> List.map snd |> List.sortDescending
    let freqs =
        match countsWithoutJokers with
        | h :: t -> (h + numberOfJokers) :: t
        | [] -> [ numberOfJokers ]
    match freqs with
    | [ 5 ] -> FiveOfAKind
    | [ 4; 1 ] -> FourOfAKind
    | [ 3; 2 ] -> FullHouse
    | [ 3; 1; 1 ] -> ThreeOfAKind
    | [ 2; 2; 1 ] -> TwoPair
    | [ 2; 1; 1; 1 ] -> OnePair
    | [ 1; 1; 1; 1; 1 ] -> HighCard
    | _ -> failwith "invalid hand"

let parseHandAndBid (l : string) = parseHand (l.Substring(0, 5)), int (l.Substring(6))

let sort  (tl, hl, _) (tr, hr, _) =
    match (compare tr tl) with
    | 0 ->
        match (List.map2 compare hr hl |> List.tryFind (fun x -> x <> 0)) with
        | Some res -> res
        | None -> 0
    | res -> res

let part1 = 
    lines
    |> List.map parseHandAndBid // parse input lines to hands and bids
    |> List.map (fun (h, b) -> (calculateHandType h, h, b)) // amend hand type
    |> List.sortWith sort // sort winning hands last
    |> List.mapi (fun i (_, _, b) -> (i + 1) * b) // multiply bid and rank
    |> List.sum
printfn "Answer part 1: %i" part1

let jackBecomesJoker = List.map (fun c -> if c = Jack then Joker else c)

let part2 = 
    lines
    |> List.map parseHandAndBid // parse input lines to hands and bids
    |> List.map (fun (h, b) -> (jackBecomesJoker h, b)) // replace jacks by jokers
    |> List.map (fun (h, b) -> (calculateHandType h, h, b)) // amend hand type
    |> List.sortWith sort // sort winning hands last
    |> List.mapi (fun i (_, _, b) -> (i + 1) * b) // multiply bid and rank
    |> List.sum
printfn "Answer part 2: %i" part2