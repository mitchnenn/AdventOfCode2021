module day_three

open System
open FSharpPlus.Control
open common

type Tally = {nbOnes:int; nbZeros:int}

let aggregate tallies n =
    tallies
    |> List.zip n
    |> List.map (fun (digit,tally) ->
        if digit = '0' then
            {tally with nbZeros = tally.nbZeros + 1}
        else
            {tally with nbOnes = tally.nbOnes + 1})

let getTallies (input:string list) = 
    input
    |> List.map(fun s -> s.ToCharArray() |> Array.toList)
    |> List.fold aggregate (List.replicate (input.Head.Length) {nbOnes=0; nbZeros=0})

let convertToDecimal binary = Convert.ToInt32(binary, 2)

let oneGreaterThanZero tally = if tally.nbOnes > tally.nbZeros then 1 else 0

let zeroGreaterThanOne tally = if tally.nbOnes < tally.nbZeros then 1 else 0

let getPowerValue f tallies =
    tallies
    |> List.map f 
    |> List.map string
    |> String.concat ""
    |> convertToDecimal

let getPowerConsumption (filename:string) =
    let input = readLines filename
    let tallies = getTallies input
    let gamma = getPowerValue oneGreaterThanZero tallies
    let epsilon = getPowerValue zeroGreaterThanOne tallies
    gamma * epsilon

let getLifeSupportValue (arrangeAndFilter:((char * int) list) -> char) input =
    let rec loop index (left:string list) =
        match left with
        | l when l |> List.length = 1 -> l.Head
                                         |> convertToDecimal
        | _ ->
            let mostCommitBit =
                left
                |> List.map(fun i -> i.ToCharArray() |> Array.toList)
                |> List.map (List.item index)
                |> List.countBy id
                |> arrangeAndFilter
            let filteredItems =
                left
                |> List.filter(fun digit -> digit.[index] = mostCommitBit)
            loop (index + 1) filteredItems                
    loop 0 input

let oxyGenBitFilter (input:(char * int) list) : char =
    input
    |> List.sortDescending
    |> List.maxBy snd
    |> fst
    
let c02SrbBitFilter (input:(char * int) list) : char =
    input
    |> List.sort
    |> List.minBy snd
    |> fst

let getLifeSupportRating (filename:string) =
    let input = readLines filename
    let oxygenGeneratorRating = getLifeSupportValue oxyGenBitFilter input
    let cO2ScrubberRating = getLifeSupportValue c02SrbBitFilter input
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
