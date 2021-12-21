module day_six

open System.IO
open System.Text
open FParsec

type Timer = {timer:int32}
type FishCount = {count:int64}
type FishState = Timer * FishCount
type Generation = FishState list

let fishParser : Parser<int32 list, unit> = sepEndBy pint32 (pstring ",") .>> spaces

let toGeneration (input:int32 list) : Generation =
    input
    |> List.countBy id
    |> List.map(fun (t,c) -> {timer=t}, {count=c})
    |> List.sortBy(fun (t,_) -> t.timer)

let parseFish filename : Generation =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Data", filename)
    let drawReply = runParserOnFile fishParser () path Encoding.UTF8
    match drawReply with
    | Success(initialFish, _, _) -> toGeneration initialFish 
    | Failure(error,_,_) -> printfn $"{error}"; List.empty
    
let addFish (gen:Generation) : Generation =
    let timerNegOne = gen
                      |> List.where(fun (t,_) -> t.timer = -1)
                      |> List.tryHead
    let tempGen = match timerNegOne with
                  | None -> gen
                  | Some(i) -> gen
                               |> List.except [timerNegOne.Value]
                               |> List.append [{timer=6}, {count=snd(i).count}]
                               |> List.append [{timer=8}, {count=snd(i).count}]
    let timerSixes = tempGen
                     |> List.where(fun (t,_) -> t.timer = 6)
    match timerSixes.IsEmpty with
    | true -> tempGen
    | false -> tempGen
               |> List.except timerSixes
               |> List.append [{timer=6}, {count=(timerSixes |> List.sumBy(fun (_,c) -> c.count))}]

let nextGeneration (gen:Generation) : Generation =
    gen
    |> List.map(fun (t,c) -> {timer=t.timer-1}, {count=c.count})
    |> addFish
    |> List.sortBy fst

let countFishInGeneration (gen:Generation) =
    gen
    |> List.map snd
    |> List.sumBy(fun i -> i.count)

let getFishCountForGeneration filename genCount =
    let rec loop n (intermediate:Generation) =
        if n = genCount + 1 then
            countFishInGeneration intermediate
        else
            loop (n+1) (nextGeneration intermediate)
    loop 1 (parseFish filename)

let runDaySix =
    let test_18 = getFishCountForGeneration "test-day-6-1.txt" 18
    printfn $"%i{test_18}"
    let prob_80 = getFishCountForGeneration "prob-day-6-2.txt" 80
    printfn $"%i{prob_80}"
    let test_256 = getFishCountForGeneration "test-day-6-1.txt" 256
    printfn $"%i{test_256}"
    let prob_256 = getFishCountForGeneration "prob-day-6-2.txt" 256
    printfn $"%i{prob_256}"
