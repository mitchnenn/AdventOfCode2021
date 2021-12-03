module common

open System.IO

let readLines (filename:string) : string seq =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", filename))
    |> Array.toSeq
