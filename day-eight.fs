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
    
let runDayEight =
    let singleNote = ["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"]
                     |> List.map toNote
    printfn "%i" (countOneFourSevenEightInOutput singleNote)
    
    let testNotes = parseNotes "test-day-8-1.txt"
    printfn "%i" (countOneFourSevenEightInOutput testNotes)
    
    let probNotes = parseNotes "prob-day-8-1.txt"
    printfn "%i" (countOneFourSevenEightInOutput probNotes)

