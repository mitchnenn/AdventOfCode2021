module day_eight

open FSharpPlus
open common

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

let lenOneFourSevenEight =
    [One |> List.length;Four |> List.length;Seven |> List.length; Eight |> List.length]
    
let countOneFourSevenEightInOutput (notes:Note list) =
    notes
    |> List.map(fun n -> n.output)
    |> List.reduce(fun state o -> state@o)
    |> List.where(fun o -> lenOneFourSevenEight |> List.contains o.Length)
    |> List.length
    
let runDayEight =
    let singleNote = ["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"]
                     |> List.map toNote
    printfn "%i" (countOneFourSevenEightInOutput singleNote)
    
    let testNotes = parseNotes "test-day-8-1.txt"
    printfn "%i" (countOneFourSevenEightInOutput testNotes)
    
    let probNotes = parseNotes "prob-day-8-1.txt"
    printfn "%i" (countOneFourSevenEightInOutput probNotes)
