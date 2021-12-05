module common

open System.IO
open System.Text

let readLines filename =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", filename))
    |> Array.map (fun s -> s.Trim())
    |> Array.toList
