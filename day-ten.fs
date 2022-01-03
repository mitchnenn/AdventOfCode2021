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

let getUnClosedDelimiters (line:string) : char list option =
    let rec loop left openChars =
        match left with
        | [] ->
            match openChars with
            | [] -> None
            | _ -> Some(openChars)
        | x::xs ->
            match x with
            | _ when isOpenChar x -> loop xs (x::openChars)
            | _ when openChars |> List.isEmpty -> None
            | _ when (getOpenChar x) <> openChars.[0] -> None
            | _ -> loop xs (openChars |> List.removeAt 0)
    loop (line |> String.toList) []

let getCompScoreValue c =
    match c with
    | ')' -> 1L
    | ']' -> 2L
    | '}' -> 3L
    | '>' -> 4L
    | _ -> failwith $"Unknown close char: {c}"

let scoreLineCompletion (comp:char list) : int64 =
    comp
    |> List.fold(fun state x -> (state * 5L) + (getCompScoreValue x)) 0L

let pickScore (scores:int64 list) : int64 =
    let temp = scores |> List.sort
    let len = scores |> List.length
    let middle = len / 2
    temp |> List.item middle

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

    let testScore2 = readLines "test-day-ten-1.txt"
                     |> List.choose(getUnClosedDelimiters)
                     |> List.map(fun l -> l |> List.map(getCloseChar))
                     |> List.map(fun l -> l |> scoreLineCompletion)
                     |> pickScore
    printfn "%A" testScore2
    
    let probScore2 = readLines "prob-day-ten-1.txt"
                     |> List.choose(getUnClosedDelimiters)
                     |> List.map(fun l -> l |> List.map(getCloseChar))
                     |> List.map(fun l -> l |> scoreLineCompletion)
                     |> pickScore
    printfn "%A" probScore2    