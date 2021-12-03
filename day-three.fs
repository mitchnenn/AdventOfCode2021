module day_three

open System
open common

let getCommon f (lines:char array seq) (col:int) =
    lines
    |> Seq.map(fun line -> line.[col])
    |> Seq.countBy id
    |> f(snd)
    |> fst

let getValue f (lines:string seq) =
    let lineChars = lines |> Seq.map(fun line -> line.ToCharArray())
    let len = lineChars |> Seq.head |> Array.length
    let rec getMostCommon' index (acc:char list) =
        match index < len with
        | false -> let value = acc
                               |> List.rev
                               |> implode
                   Convert.ToInt32(value, 2)
        | true -> getMostCommon' (index + 1) ((getCommon f lineChars index) :: acc)
    getMostCommon' 0 []

let getPowerConsumption (filename:string) =
    let lines = readLines filename
    match lines with
    | s when Seq.isEmpty s -> -1
    | _ ->
           let gamma = getValue Seq.maxBy lines
           let epsilon = getValue Seq.minBy lines
           gamma * epsilon

let runDayThree =
    // Part 1
    let testPowerConsumption = getPowerConsumption "test-input-3-1.txt"
    printfn $"%A{testPowerConsumption}"
    let probPowerConsumption = getPowerConsumption "prob-input-3-1.txt"
    printfn $"%A{probPowerConsumption}"
    // Part 2
