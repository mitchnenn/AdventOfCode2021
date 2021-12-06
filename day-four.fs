module day_four

open System.IO
open System.Text
open FParsec
open fparsec_helper

let drawParser : Parser<int list, unit> = sepEndBy pint32 (pstring ",") .>> spaces;;

let parseDraws filename = 
    let drawReply = runParserOnFile drawParser () filename Encoding.UTF8
    match drawReply with
    | Success(draws, _, _) -> draws
    | Failure(_,_,_) -> List.empty

let ns : Parser<string,unit> = str "  " <|> str " " <|> str ""
let boardNumbers = many1 (pint32 .>> ns)
let boardRow = boardNumbers .>> ws
let boardParser = drawParser >>. manyTill boardRow eof
    
let parseAllBoardRows filename =
    let reply = runParserOnFile boardParser () filename Encoding.UTF8
    match reply with
    | Success(boards, _, _) -> boards
    | Failure(_,_,_) -> List.empty

let parseInput filename =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, "Data", filename)
    let draws = parseDraws filename
    let allBoardRows = parseAllBoardRows filename
    let len = allBoardRows |> List.head |> List.length
    let boards = allBoardRows
                 |> List.chunkBySize len
    draws, boards, len

let findWinningBoard currentDraws boards len : int32 =
    2

let playBingo filename =
    let draws, boards, len = parseInput filename
    let rec loop currentDrawNumber =
        match currentDrawNumber with
        | i when i = draws.Length + 1 -> ([], -1)
        | _ ->
            let currentDraws = draws |> List.take currentDrawNumber
            let winningIndex = findWinningBoard currentDraws boards len
            match winningIndex <= 0 with
            | false -> (currentDraws, winningIndex)
            | true -> loop (currentDrawNumber + 1)
    let currentDraws, winningBoardIndex = loop len
    currentDraws,winningBoardIndex

let runDayFour =
    let result = playBingo "test-input-4-1.txt"
    printfn $"%A{result}"
