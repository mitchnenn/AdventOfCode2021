module day_nine

open FParsec
open common

let digitParser : Parser<char list, unit> = many digit .>> spaces

let charToInt c = c |> sprintf "%c" |> int

let parseDigits (line:string) : int32 list =
    let reply = runParserOnString digitParser () "" line
    match reply with
    | Success(digits, _, _) -> digits |> List.map(charToInt)
    | Failure(error,_,_) -> printfn $"{error}"; List.empty
    
let toDigitArray (allDigits:int32 list list) = 
    Array2D.init
         (allDigits |> List.length)
         allDigits.[0].Length
         (fun i j -> allDigits.[i].[j])

let isIndexInRange (heightMap:int32[,]) (i,j) =
    let width = heightMap |> Array2D.length1
    let height = heightMap |> Array2D.length2
    i >= 0 && i < width && j >= 0 && j < height

let neighbors (heightMap:int32[,]) (i,j) =
    match (i,j) with
    | _ when not(isIndexInRange heightMap (i,j)) -> []
    | _ -> [(-1,0);(0,-1);(1,0);(0,1)]
            |> List.map(fun (di,dj) -> (i+di, j+dj) )
            |> List.where(isIndexInRange heightMap)
            |> List.map(fun (ii,jj) -> heightMap[ii,jj])
        
let lowPoint (heightMap:int32[,]) (i,j) v : int32 option =
    let ns = neighbors heightMap (i,j)
    match ns with
    | _ when ns |> List.forall(fun n -> v < n) -> Some(v)
    | _ -> None

let rec getLowPoints (heightMap:int32[,]) =
    heightMap
    |> Array2D.mapi(fun i j -> lowPoint heightMap (i,j))
    |> Seq.cast<int32 option>
    |> Seq.choose id
    |> Seq.map(fun l -> l + 1)
    |> Seq.sum

let runDayNine =
    let testLowPoints = readLines "test-day-nine-1.txt"
                        |> List.map(parseDigits)
                        |> toDigitArray
                        |> getLowPoints
    printfn "%A" testLowPoints
    
    