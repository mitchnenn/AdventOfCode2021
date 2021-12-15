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

let getHLinePoints (line:Point * Point) : Point list =
    let ys = [fst(line).y; snd(line).y] 
    [(ys |> List.min)..(ys |> List.max)]
    |> List.map(fun y -> {x=fst(line).x; y=y})

let getAllHorizontalPoints hLines =
    hLines
    |> List.map getHLinePoints
    |> List.reduce (fun i state -> state@i)

let getVerticalLines points = points |> List.where(fun (s,e) -> s.y = e.y)

let getVLinePoints (line:Point * Point) : Point list =
    let xs = [fst(line).x; snd(line).x]
    [(xs |> List.min)..(xs |> List.max)]
    |> List.map(fun x -> {x=x; y=fst(line).y})
    
let getAllVerticalPoints vLines =
    vLines
    |> List.map getVLinePoints
    |> List.reduce (fun i state -> state@i)

let getDiagonalLines points =
    points
    |> List.where(fun (s,e) -> abs(s.x - e.x) = abs(s.y - e.y))

let enumInts first last =
    match first - last with
    | diff when diff > 0 -> seq { for i = first downto last do yield i }
    | diff when diff < 0 -> seq { for i = first to last do yield i }
    | _ -> failwith "Can't create list of ints."

let getDLinePoints (line:Point * Point) : Point list =
    let xs = enumInts (fst(line).x) (snd(line).x) |> Seq.toList
    let ys = enumInts (fst(line).y) (snd(line).y) |> Seq.toList
    List.zip xs ys
    |> List.map(fun (x,y) -> {x=x; y=y})

let getAllDiagonalPoints dLines =
    dLines
    |> List.map getDLinePoints
    |> List.reduce (fun i state -> state@i)

let getDangerPointCount submarineLines =
    let points = toPoints submarineLines
    let hPoints = getHorizontalLines points
                  |> getAllHorizontalPoints
    let vPoints = getVerticalLines points
                  |> getAllVerticalPoints
    let dPoints = getDiagonalLines points
                  |> getAllDiagonalPoints
    [hPoints;vPoints;dPoints]
    |> List.concat
    |> List.countBy id
    |> List.where(fun (_,c) -> c >= 2)
    |> List.length

let runDayFive =
    let testSubmarineLines = parseInputFile "test-input-5-1.txt"
    let testPointsCoveredCount = getDangerPointCount testSubmarineLines
    printfn $"%i{testPointsCoveredCount}"        
    let probSubmarineLines = parseInputFile "test-input-5-2.txt"
    let probPointsCoveredCount = getDangerPointCount probSubmarineLines
    printfn $"%i{probPointsCoveredCount}"        
    