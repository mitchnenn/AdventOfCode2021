module day_eight

open FSharpPlus
open common

let Zero = "abcefg" 
let One = "cf" 
let Two = "acdeg" 
let Three = "acdfg" 
let Four = "bcdf" 
let Five = "abdfg" 
let Six = "abdefg" 
let Seven = "acf" 
let Eight = "abcdefg" 
let Nine = "abcdfg" 

let toDigit (input:string) : string =
    if input = Zero then "0"
    else if input = One then "1"
    else if input = Two then "2"
    else if input = Three then "3"
    else if input = Four then "4"
    else if input = Five then "5"
    else if input = Six then "6"
    else if input = Seven then "7"
    else if input = Eight then "8"
    else if input = Nine then "9"
    else failwith $"Unknown number: {input}"

type Note = {pattern:string list; output:string list}

let signalIsNotDelimiter s = s <> "|"

let toNote (input:string) : Note = 
    let splitString = input.Split " " |> Array.toList
    let signals = splitString
                  |> List.takeWhile signalIsNotDelimiter
                  |> List.map id
    let output = splitString
                 |> List.skipWhile signalIsNotDelimiter
                 |> List.skip 1
                 |> List.map id
    {pattern=signals; output=output}
                 
let parseNotes filename : Note list = readLines filename |> List.map toNote

let lenOneFourSevenEight = [One.Length;Four.Length;Seven.Length; Eight.Length]
    
let countOneFourSevenEightInOutput (notes:Note list) =
    notes
    |> List.collect(fun n -> n.output)
    |> List.where(fun o -> lenOneFourSevenEight |> List.contains o.Length)
    |> List.length

let findStrByLen pattern len =
    pattern
    |> List.filter(fun s -> (s |> String.length) = len)

let getCharMapFromPatternByLen pattern input =
    let len = input |> String.length
    let observation = findStrByLen pattern len |> List.head
    observation, input    
    
let removeChars input charsToStrip = 
    input
    |> String.toList
    |> List.except (charsToStrip |> String.toList)
    |> List.map string
    |> List.reduce(+)
    
let getCandFSegMap pattern =
    getCharMapFromPatternByLen pattern One

let removeSegments digitSegMap segmentsToRemove = 
    let observations = removeChars (fst(digitSegMap)) (fst(segmentsToRemove))
    let segments = removeChars (snd(digitSegMap)) (snd(segmentsToRemove))
    observations, segments

let getBandDSegMap pattern cAndFSeqMap =
    let fourSegMap = getCharMapFromPatternByLen pattern Four
    removeSegments fourSegMap cAndFSeqMap
    
let getASegMap pattern cAndFSeqMap = 
    let sevenSegMap = getCharMapFromPatternByLen pattern Seven
    removeSegments sevenSegMap cAndFSeqMap

let containsSegment segment target =
    segment
    |> String.toList
    |> List.forall(fun c -> target
                            |> String.toList
                            |> List.contains c)

let determineDigitOfLenFive pattern (signal:string) =
    let cAndFSegments = getCandFSegMap pattern
    let bAndDSegments = getBandDSegMap pattern cAndFSegments
    let aSegment = getASegMap pattern cAndFSegments
    match signal with
    | _ when containsSegment (fst(cAndFSegments)) signal -> "3"
    | _ when containsSegment (fst(bAndDSegments)) signal -> "5"
    | _ -> "2"

let determineDigitOfLenSix pattern (signal:string) =
    let cAndFSegments = getCandFSegMap pattern
    let bAndDSegments = getBandDSegMap pattern cAndFSegments
    match signal with
    | _ when (containsSegment (fst(cAndFSegments)) signal) && (containsSegment (fst(bAndDSegments)) signal) -> "9"
    | _ when (containsSegment (fst(bAndDSegments)) signal) -> "6"
    | _ -> "0"
    
let decodeOutputDigit (pattern:string list) (signal:string) =
    match signal.Length with
    | 2 -> "1"
    | 3 -> "7"
    | 4 -> "4"
    | 5 -> determineDigitOfLenFive pattern signal
    | 6 -> determineDigitOfLenSix pattern signal
    | 7 -> "8"
    | _ -> failwith $"Unexpected digit length: {signal.Length}"

let decodeNote note =
    let pattern = note.pattern
    let output = note.output
    output
    |> List.map(decodeOutputDigit pattern)
    |> List.reduce(+)

let decodeNoteOutputs (notes:Note list) =
    notes
    |> List.map decodeNote

let runDayEight =
    let singleNote = ["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"]
                     |> List.map toNote
//    printfn "%i" (countOneFourSevenEightInOutput singleNote)
//    
//    let testNotes = parseNotes "test-day-8-1.txt"
//    printfn "%i" (countOneFourSevenEightInOutput testNotes)
//    
//    let probNotes = parseNotes "prob-day-8-1.txt"
//    printfn "%i" (countOneFourSevenEightInOutput probNotes)

    printfn "%A" (decodeNoteOutputs singleNote)

    let testNote = ["fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"]
                   |> List.map toNote
    printfn "%A" (decodeNoteOutputs testNote)
    
    let testNotes2Sum = parseNotes "test-day-8-1.txt"
                        |> decodeNoteOutputs
                        |> List.map int32
                        |> List.sum
    printfn "%i" testNotes2Sum
    
    let probNotes2Sum = parseNotes "prob-day-8-1.txt"
                        |> decodeNoteOutputs
                        |> List.map int32
                        |> List.sum
    printfn "%i" probNotes2Sum
