module common

open System.IO
open System.Text

let readLines (filename:string) : string seq =
    File.ReadAllLines(Path.Combine(__SOURCE_DIRECTORY__, "Data", filename))
    |> Array.toSeq

let implode (x:char list) =
    let sb = StringBuilder(x.Length)
    x |> List.iter (sb.Append >> ignore)
    sb.ToString()
    