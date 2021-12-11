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

let hasWinningNumbers currentDraws boardRowsOrColumns = 
    let sortedDraws = currentDraws |> List.sort
    (boardRowsOrColumns
     |> List.where(fun br -> (br
                              |> List.sort
                              |> List.except sortedDraws
                              |> List.length) = 0)
     |> List.length) > 0

let hasWinningCol currentDraws (board:int32 list list) : bool =
    let boardArray = array2D board
    let len = boardArray |> Array2D.length2
    let rec loop index acc =
        match index = len with
        | true -> acc |> List.rev
        | false ->
            let col : int32 list = boardArray[*,index] |> Array.toList
            loop (index + 1) (col::acc)
    loop 0 []
    |> hasWinningNumbers currentDraws
        
let hasWinningRow (currentDraws:int32 list) (board:int32 list list) : bool =
    hasWinningNumbers currentDraws board

let findWinningBoard currentDraws (boards:int32 list list list) len : int32 =
    let rec loop boardIndex =
        match boardIndex = (boards |> List.length) with
        | true -> -1
        | false ->
            let winningColFound = hasWinningCol currentDraws boards.[boardIndex]
            let winningRowFound = hasWinningRow currentDraws boards.[boardIndex]
            match winningColFound || winningRowFound with
            | true -> boardIndex
            | false -> loop (boardIndex + 1)
    loop 0

let calcScore (draws:int32 list) (board:int32 list list) : int32 =
    let notPickedSum = board
                       |> List.reduce(fun state i -> state @ i)
                       |> List.except draws
                       |> List.sum
    (draws |> List.last) * notPickedSum

let playBingo filename =
    let draws, boards, len = parseInput filename
    let rec loop drawIndex =
        match drawIndex = (draws |> List.length) with
        | true -> ([], -1)
        | false ->
            let currentDraws = draws.[0..drawIndex]
            let winningIndex = findWinningBoard currentDraws boards len
            match winningIndex < 0 with
            | false -> (currentDraws, winningIndex)
            | true -> loop (drawIndex + 1)
    let currentDraws, winningBoardIndex = loop (len - 1)
    calcScore currentDraws boards.[winningBoardIndex]

let runDayFour =
    // Part 1.
    let testInputResult = playBingo "test-input-4-1.txt"
    printfn $"%A{testInputResult}"
    let probInputResult = playBingo "prob-input-4-2.txt"
    printfn $"%A{probInputResult}"
    // Part 2.
    