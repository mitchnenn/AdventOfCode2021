module day_eight

open FSharpPlus

let Zero = "abcefg" |> String.toList
let One = "cf" |> String.toList
let Two = "acdeg" |> String.toList
let Three = "acdfg" |> String.toList
let Four = "bcdf" |> String.toList
let Five = "abdfg" |> String.toList
let Six = "abdefg" |> String.toList
let Seven = "acf" |> String.toList
let Eight = "abcdefg" |> String.toList
let Nine = "abcdfg" |> String.toList

open common

type Note = {pattern:char list list; output:char list list}

let signalIsNotDelimiter s = s <> "|"

let toNote (input:string) : Note = 
    let splitString = input.Split " " |> Array.toList
    let signals = splitString
                  |> List.takeWhile signalIsNotDelimiter
                  |> List.map id
                  |> List.map(fun i -> i |> String.toList)
    let output = splitString
                 |> List.skipWhile signalIsNotDelimiter
                 |> List.skip 1
                 |> List.map id
                 |> List.map(fun i -> i |> String.toList)
    {pattern=signals; output=output}
                 
let parseNotes filename : Note list = readLines filename |> List.map toNote

let findInPatternByLength n l = n.pattern |> List.find(fun s -> s.Length = l)

let getEMap (input:(char * char) list) =
    let missingMapValue = Eight
                          |> List.except (input |> List.map snd)
                          |> List.head
    ['e', missingMapValue]
    
let createSegMap note : (char * char) list =
    let noteSevenMap = (findInPatternByLength note 3) |> List.zip Seven
    let noteFourMap = (findInPatternByLength note 4) |> List.zip Four
    let noteEight = findInPatternByLength note 7 
    let noteEightMap = List.zip [noteEight; Eight]
    let notedWoEMap = List.concat [noteSevenMap; noteFourMap; noteEightMap]
                      |> List.distinctBy fst
    notedWoEMap
    |> List.append (getEMap notedWoEMap)
    |> List.sortBy fst

let runDayEight =
    //let notes = parseNotes "test-day-8-1.txt"
    let singleNote = ["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"]
                     |> List.map toNote
                     |> List.head
                     
    
    printfn "%A" ((createSegMap singleNote) |> List.sortBy snd)
