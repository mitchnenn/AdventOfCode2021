module day_three

open System
open common

let getMostCommon (lines:char array seq) (col:int) =
    lines
    |> Seq.map(fun line -> line.[col])
    |> Seq.countBy id
    |> Seq.maxBy(snd)
    |> fst

let getGamma (lines:string seq) =
    let lineChars = lines |> Seq.map(fun line -> line.ToCharArray())
    let len = lineChars |> Seq.head |> Array.length
    let rec getMostCommon' index (acc:char list) =
        match index < len with
        | false -> let value = acc
                               |> List.rev
                               |> implode
                   Convert.ToInt32(value, 2)
        | true -> getMostCommon' (index + 1) ((getMostCommon lineChars index) :: acc)
    getMostCommon' 0 []

let getLeastCommon (lines:char array seq) (col:int) =
    lines
    |> Seq.map(fun line -> line.[col])
    |> Seq.countBy id
    |> Seq.minBy(snd)
    |> fst

let getEpsilon (lines:string seq) =
    let lineChars = lines |> Seq.map(fun line -> line.ToCharArray())
    let len = lineChars |> Seq.head |> Array.length
    let rec getLeastCommon' index (acc:char list) =
        match index < len with
        | false -> let value = acc
                               |> List.rev
                               |> implode
                   Convert.ToInt32(value, 2)
        | true -> getLeastCommon' (index + 1) ((getLeastCommon lineChars index) :: acc)
    getLeastCommon' 0 []

let getPowerConsumption (filename:string) =
    let lines = readLines filename
    match lines with
    | s when Seq.isEmpty s -> 0
    | _ -> let gamma = getGamma lines
           let epsilon = getEpsilon lines
           gamma * epsilon

let runDayThree =
    // Part 1
    let testPowerConsumption = getPowerConsumption "test-input-3-1.txt"
    printfn $"%A{testPowerConsumption}"
    let probPowerConsumption = getPowerConsumption "prob-input-3-1.txt"
    printfn $"%A{probPowerConsumption}"
    // Part 2
