module day_ten

open FSharpPlus
open common

let delimiters = [('(',')');('[',']');('{','}');('<','>')]

let isOpenChar c = delimiters
                   |> List.map fst
                   |> List.contains c

let getOpenChar closeChar = delimiters
                            |> List.find(fun (_,c) -> c = closeChar)
                            |> fst

let getCloseChar openChar = delimiters
                            |> List.find(fun (o,_) -> o = openChar)
                            |> snd
 
let findCorruptChar (line:string) : char option =
    let rec loop left openChars =
        match left with
        | [] -> None
        | x::xs ->
            match x with
            | _ when isOpenChar x -> loop xs (x::openChars)
            | _ when openChars |> List.isEmpty -> Some(x)
            | _ when (getOpenChar x) <> openChars.[0] -> Some(x)
            | _ -> loop xs (openChars |> List.removeAt 0)
    loop (line |> String.toList) []

let getCorruptCharScore illegalChar =
    match illegalChar with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith $"Not a valid close character: {illegalChar}"

let runDayTen =
    let testScore1 = readLines "test-day-ten-1.txt"
                     |> List.choose(findCorruptChar)
                     |> List.map(getCorruptCharScore)
                     |> List.sum
    printfn "%i" testScore1
    
    let probScore1 = readLines "prob-day-ten-1.txt"
                     |> List.choose(findCorruptChar)
                     |> List.map(getCorruptCharScore)
                     |> List.sum
    printfn "%i" probScore1
