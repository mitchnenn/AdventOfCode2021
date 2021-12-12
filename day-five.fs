module day_five

open System.Text
open System.IO
open FParsec
open fparsec_helper

let parsePoint : Parser<int32 * int32,unit> = tuple2 (pint32 .>> pchar ',') pint32

let parseLineTo : Parser<string,unit> = str " -> "

let parseInputLine : Parser<(int32 * int32) * (int32 * int32),unit> =
    tuple2 (parsePoint .>> parseLineTo) (parsePoint .>> spaces)

let inputLineParsers = many parseInputLine 

let parseInputFile filename = 
    let path = Path.Combine(__SOURCE_DIRECTORY__, "Data", filename)
    let drawReply = runParserOnFile inputLineParsers () path Encoding.UTF8
    match drawReply with
    | Success(submarineLines, _, _) -> submarineLines
    | Failure(error,_,_) -> printfn $"{error}"; List.empty

type Point = {x:int32; y:int32}

let toPoints subLines =
    subLines
    |> List.map(fun (p1,p2) -> {x=fst(p1);y=snd(p1)}, {x=fst(p2);y=snd(p2)} )

let getAllPoints points =
    let allStart = points |> List.map fst
    let allEnd = points |> List.map snd
    [allStart;allEnd] |> List.concat

let getDimensions points =
    let width = getAllPoints points
                |> List.map (fun p -> p.x)
                |> List.max
    let height = getAllPoints points 
                 |> List.map (fun p -> p.y)
                 |> List.max
    width, height

let getHorizontalLines points = points |> List.where(fun (s,e) -> s.x = e.x)
let getVerticalLines points = points |> List.where(fun (s,e) -> s.y = e.y)

let getHLinePoints (line:Point * Point) : Point list =
    let ys = [fst(line).y; snd(line).y] 
    [(ys |> List.min)..(ys |> List.max)]
    |> List.map(fun y -> {x=fst(line).x; y=y})

let getVLinePoints (line:Point * Point) : Point list =
    let xs = [fst(line).x; snd(line).x]
    [(xs |> List.min)..(xs |> List.max)]
    |> List.map(fun x -> {x=x; y=fst(line).y})
    
let getAllHorizontalPoints hLines =
    hLines
    |> List.map getHLinePoints
    |> List.reduce (fun i state -> state@i)

let getAllVerticalPoints vLines =
    vLines
    |> List.map getVLinePoints
    |> List.reduce (fun i state -> state@i)

let getDangerPointCount submarineLines =
    let points = toPoints submarineLines
    let hPoints = getHorizontalLines points
                  |> getAllHorizontalPoints
    let vPoints = getVerticalLines points
                  |> getAllVerticalPoints
    [hPoints;vPoints]
    |> List.concat
    |> List.countBy id
    |> List.where(fun (_,c) -> c >= 2)
    |> List.length

let runDayFive =
    // Part 1.
    let testSubmarineLines = parseInputFile "test-input-5-1.txt"
    let testPointsCoveredCount = getDangerPointCount testSubmarineLines
    let probSubmarineLines = parseInputFile "test-input-5-2.txt"
    let probPointsCoveredCount = getDangerPointCount probSubmarineLines
    printf $"%A{probPointsCoveredCount}"        
    // Part 2.
    