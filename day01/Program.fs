open System.IO

let input = File.ReadAllLines("input.txt")

let alfaDigits = [| ("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9) |]
let numericDigits = [| ("1", 1); ("2", 2); ("3", 3); ("4", 4); ("5", 5); ("6", 6); ("7", 7); ("8", 8); ("9", 9) |]
let alfaNumericDigits = Array.concat [alfaDigits; numericDigits]

let calibrationValue (digits:(string * int) array) (s:string) =
    let firstDigit =
        digits
        |> Array.map (fun t -> (s.IndexOf(fst t), snd t))
        |> Array.filter (fun t -> fst t > -1)
        |> Array.minBy fst
        |> snd
    let lastDigit =
        digits
        |> Array.map (fun t -> (s.LastIndexOf(fst t), snd t))
        |> Array.maxBy fst
        |> snd
    firstDigit * 10 + lastDigit

let part1 = input |> Array.map (calibrationValue numericDigits) |> Array.sum
printfn "Answer part 1: %i" part1

let part2 = input |> Array.map (calibrationValue alfaNumericDigits) |> Array.sum
printfn "Answer part 2: %i" part2