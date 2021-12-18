module day_six

open System.IO
open System.Text
open FParsec

let lanFishParser : Parser<int32 list, unit> =
    sepEndBy pint32 (pstring ",") .>> spaces

let getInitialLanFish filename : int32 list =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Data", filename)
    let drawReply = runParserOnFile lanFishParser () path Encoding.UTF8
    match drawReply with
    | Success(initialLanFish, _, _) -> initialLanFish 
    | Failure(error,_,_) -> printfn $"{error}"; List.empty

let decFishTimer = List.map(fun t -> t - 1)

let fishCountToAdd fish = 
    fish
    |> List.where(fun t -> t = 0)
    |> List.length
    
let addNewFish fishToAdd fish =
    fish @ ((seq{for _ = 1 to fishToAdd do yield 8}) |> Seq.toList)

let resetFishTimers =  List.map(fun t -> if t < 0 then 6 else t)

let getLanFishCount filename days =
    let initialFish = getInitialLanFish filename
    let rec loop day fishToAdd fish =
        match day > days with
        | true -> fish |> List.length
        | false ->
                  let decFish = fish
                                |> decFishTimer
                  let nextFishToAdd = fishCountToAdd decFish
                  let nextFish = decFish
                                 |> addNewFish fishToAdd
                                 |> resetFishTimers
                  loop (day+1) nextFishToAdd nextFish
    loop 1 0 initialFish

let runDaySix =
    let test_18 = getLanFishCount "test-day-6-1.txt" 18
    printfn $"%i{test_18}"
    let prob_80 = getLanFishCount "prob-day-6-2.txt" 80
    printfn $"%i{prob_80}"
    let prob_80 = getLanFishCount "test-day-6-1.txt" 256
    printfn $"%i{prob_80}"
