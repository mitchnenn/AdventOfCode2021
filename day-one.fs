module day_one

open System.IO

let countIncreasingReadings (filename:string) : int =
    let readings = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", filename))
    match readings with
    | [||] -> 0
    | _ -> readings
           |> Seq.map(fun i -> i |> int64)
           |> Seq.pairwise
           |> Seq.countBy(fun (f,s) -> (s - f) > 0L)
           |> Seq.where(fun (b,i) -> b = true)
           |> Seq.map snd
           |> Seq.head

let countIncreasingSlidingWindows (filename:string) =
    let readings = File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", filename))
    match readings with
    | [||] -> 0
    | _ -> readings
           |> Seq.map(fun s -> s |> int)
           |> Seq.windowed 3
           |> Seq.map Seq.sum
           |> Seq.pairwise
           |> Seq.countBy(fun (f,s) -> (s - f) > 0)
           |> Seq.where(fun (b,i) -> b = true)
           |> Seq.map snd
           |> Seq.head

let runDayOne =
    // Part 1
    let testReadings = countIncreasingReadings @"test-input-1.txt"
    printfn $"Test: %i{testReadings}"
    let readings = countIncreasingReadings @"prob-input-1.txt"
    printfn $"Problem: %i{readings}"
    // Part 2
    let testWindows = countIncreasingSlidingWindows @"test-input-1-2.txt"
    printfn $"Test: %i{testWindows}"
    let windows = countIncreasingSlidingWindows @"prob-input-1-2.txt"
    printfn $"Problem: %i{windows}"
    
