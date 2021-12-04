module day_three

open System
open common

let getCountsByColumn (lines:char array seq) (col:int) =
    lines
    |> Seq.map(fun line -> line.[col])
    |> Seq.countBy id

let getCommon f (lines:char array seq) (col:int) =
    getCountsByColumn lines col
    |> f(snd)
    |> fst

let getValue f (lines:string seq) =
    let lineChars = lines |> Seq.map(fun line -> line.ToCharArray())
    let len = lineChars |> Seq.head |> Array.length
    let rec getCommon' index (acc:char list) =
        match index < len with
        | false -> let value = acc
                               |> List.rev
                               |> implode
                   Convert.ToInt32(value, 2)
        | true -> getCommon' (index + 1) ((getCommon f lineChars index) :: acc)
    getCommon' 0 []

let getPowerConsumption (filename:string) =
    let lines = readLines filename
    match lines with
    | s when Seq.isEmpty s -> -1
    | _ ->
           let gamma = getValue Seq.maxBy lines
           let epsilon = getValue Seq.minBy lines
           gamma * epsilon

let getMostCommonByColumn f (input:(char*int) []) (def:char) : char =
    match input with
    | a when a.Length <= 0 -> def
    | a when a.Length = 1 -> fst(a.[0])
    | _ ->
    match snd(input.[0]) = snd(input.[1]) with
        | true -> def
        | false -> input
                |> f(snd)
                |> fst

let getOxygenGeneratorRating f (input:string seq) (def:char) =
    let lineChars = input |> Seq.map(fun line -> line.ToCharArray())
    let len = lineChars |> Seq.head |> Array.length
    let rec loop (col:int) (current:seq<char []>) =
        match col = len with
        | true ->
            match current |> Seq.length with
            | 0 -> "0"
            | _ -> current
                   |> Seq.head
                   |> Array.toList
                   |> implode
        | _ ->
            match current |> Seq.length with
            | 1 -> current
                   |> Seq.head
                   |> Array.toList
                   |> implode
            | _ ->
                let counts = getCountsByColumn current col |> Seq.toArray
                let mostCommon = getMostCommonByColumn f counts def
                let toInclude = current
                                |> Seq.where(fun i -> i.[col] = mostCommon)
                loop (col + 1) toInclude
    let value =loop 0 lineChars
    Convert.ToInt32(value, 2)

let getLifeSupportRating (filename:string) =
    let lines = readLines filename
    match lines with
    | s when Seq.isEmpty s -> -1
    | _ ->
           let oxygenGeneratorRating = getOxygenGeneratorRating Seq.maxBy lines '1'
           let cO2ScrubberRating = getOxygenGeneratorRating Seq.minBy lines '0'
           oxygenGeneratorRating * cO2ScrubberRating

let runDayThree =
    // Part 1
    let testPowerConsumption = getPowerConsumption "test-input-3-1.txt"
    printfn $"%A{testPowerConsumption}"
    let probPowerConsumption = getPowerConsumption "prob-input-3-1.txt"
    printfn $"%A{probPowerConsumption}"
    // Part 2
    let testLifeSupportRating = getLifeSupportRating "test-input-3-1.txt"
    printfn $"%A{testLifeSupportRating}"
    let probLifeSupportRating = getLifeSupportRating "prob-input-3-1.txt"
    printfn $"%A{probLifeSupportRating}"
