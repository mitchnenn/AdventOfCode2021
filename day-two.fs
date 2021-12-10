module day_two

open System
open System.IO

open common

type Command = {command:string; amount:int;}
type Position = {position:string; amount:int;}

let getPosition (filename:string) : int =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", filename))
    |> Seq.map(fun line -> line.Split(" ", StringSplitOptions.None))
    |> Seq.map(fun values -> {command=values.[0]; amount=values.[1] |> int})
    |> Seq.groupBy(fun c -> c.command)
    |> Seq.map(fun (c,a) -> {command=c; amount=a |> Seq.sumBy(fun v -> v.amount)})
    |> Seq.map(fun c ->
       match c.command with
       | "forward" -> {position="horizontal"; amount=c.amount}
       | "up" -> {position="depth"; amount=c.amount*(-1)}
       | "down" -> {position="depth"; amount=c.amount})
    |> Seq.groupBy(fun p -> p.position)
    |> Seq.map(fun (p,ps) -> {position=p; amount=ps |> Seq.sumBy(fun v -> v.amount)})
    |> Seq.map(fun p -> p.amount)                   
    |> Seq.reduce(fun state s -> state * s) 

let getPositionWithAim (filename:string) =
    let position = readLines filename
                   |> Seq.map(fun line -> line.Split(" "))
                   |> Seq.map(fun i -> {command=i.[0]; amount=(i.[1] |> int)})
                   |> Seq.fold(fun (horizontal,depth,aim) i -> match i.command with
                                                               | "forward" -> (horizontal + i.amount, depth + (aim * i.amount), aim)
                                                               | "down" -> (horizontal, depth, aim + i.amount)
                                                               | "up" -> (horizontal, depth, aim - i.amount)) (0,0,0)
    let (horizontal,depth,_) = position
    horizontal * depth
    
let runDayTwo =
    // Part 1.
    let testCommands = getPosition "test-input-2-1.txt"
    printfn $"%A{testCommands}"
    let probCommands = getPosition "prob-input-2-1.txt"
    printfn $"%A{probCommands}"
    // Part 2.
    let testCommands2 = getPositionWithAim "test-input-2-2.txt"
    printfn $"%A{testCommands2}"
    let probCommands2 = getPositionWithAim "prob-input-2-2.txt"
    printfn $"%A{probCommands2}"
