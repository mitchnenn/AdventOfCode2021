module day_one

open System.IO

let countIncreasingReadings (filename:string) : int =
    let readings = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, filename))
    match readings with
    | [||] -> 0
    | _ -> readings
           |> Seq.map(fun i -> i |> int64)
           |> Seq.pairwise
           |> Seq.countBy(fun (f,s) -> (s - f) > 0L)
           |> Seq.where(fun (b,i) -> b = true)
           |> Seq.map snd
           |> Seq.head

let runDayOne =
    let testReadings = countIncreasingReadings @"test-input-1.txt"
    printfn $"Test readings: %i{testReadings}"
    let readings = countIncreasingReadings @"prob-input-1.txt"
    printfn $"Problem Input: %i{readings}" 
