module day_seven

open System.IO
open System.Text
open FParsec

let crabPositionParser : Parser<int32 list, unit> = sepEndBy pint32 (pstring ",") .>> spaces

let parseCrabPositions filename : int32 list =
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Data", filename)
    let drawReply = runParserOnFile crabPositionParser () path Encoding.UTF8
    match drawReply with
    | Success(crabPositions, _, _) -> crabPositions
    | Failure(error,_,_) -> printfn $"{error}"; List.empty

let calcFuel (crabPos:int32 list) (pos:int32) : int32 =
    crabPos
    |> List.sumBy(fun p -> abs(p - pos))

let calcFuelNonConstantInterval (pos:int32) (crabPos:int32) : int32 =
    let intervals = [1..abs(pos - crabPos)]
    match (intervals |> List.length) <= 0 with
    | true -> 0
    | false -> intervals |> List.reduce(+)

let calcFuelNonConstant (crabPos:int32 list) (pos:int32) : int32 =
   crabPos
   |> List.map(calcFuelNonConstantInterval pos)
   |> List.sum

let calcMinFuel calcFunc (crabPos:int32 list) : int32 = 
    [(crabPos |> List.min)..(crabPos |> List.max)]
    |> List.map(calcFunc crabPos)
    |> List.min

let runDaySeven =
    let testDay_7_1 = parseCrabPositions "test-day-7-1.txt" |> calcMinFuel calcFuel
    printfn $"%i{testDay_7_1}"
    let probDay_7_1 = parseCrabPositions "prob-day-7-1.txt" |> calcMinFuel calcFuel
    printfn $"%i{probDay_7_1}"
    let testDay_7_2 = parseCrabPositions "test-day-7-1.txt" |> calcMinFuel calcFuelNonConstant
    printfn $"%i{testDay_7_2}"
    let probDay_7_2 = parseCrabPositions "prob-day-7-1.txt" |> calcMinFuel calcFuelNonConstant
    printfn $"%i{probDay_7_2}"
