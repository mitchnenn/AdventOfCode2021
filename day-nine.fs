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

let getNeighborsi (heightMap:int32[,]) (i,j) =
    match (i,j) with
    | _ when not(isIndexInRange heightMap (i,j)) -> []
    | _ -> [(-1,0);(0,-1);(1,0);(0,1)]
            |> List.map(fun (di,dj) -> (i+di, j+dj) )
            |> List.where(isIndexInRange heightMap)

/////////////////////// Part 1 //////////////////////////////////

let neighborValues (heightMap:int32[,]) (i,j) =
    getNeighborsi heightMap (i,j)
    |> List.map(fun (ii,jj) -> heightMap[ii,jj])
        
let getLowPointValue (heightMap:int32[,]) (i,j) v : int32 option =
    let ns = neighborValues heightMap (i,j)
    match ns with
    | _ when ns |> List.forall(fun n -> v < n) -> Some(v)
    | _ -> None

let sumRisksForLowPoints (heightMap:int32[,]) =
    heightMap
    |> Array2D.mapi(fun i j -> getLowPointValue heightMap (i,j))
    |> Seq.cast<int32 option>
    |> Seq.choose id
    |> Seq.map(fun l -> l + 1)
    |> Seq.sum
    
let getLowPointi (heightMap:int32[,]) (i,j) v : (int32 * int32) option =
    let ns = neighborValues heightMap (i,j)
    match ns with
    | _ when ns |> List.forall(fun n -> v < n) -> Some(i,j)
    | _ -> None

/////////////////////// Part 2 //////////////////////////////////

let neighborsExceptByValue (heightMap:int32[,]) (i,j) v =
    getNeighborsi heightMap (i,j)
    |> List.where(fun (ii,jj) -> heightMap[ii,jj] <> v)

let getBasin (heightMap:int32[,]) (i,j) =
    let rec loop pointsLeft acc = 
        match pointsLeft with
        | [] -> acc 
        | n::ns ->
            let newAcc = n::acc
                         |> List.distinct
            let newPoints = neighborsExceptByValue heightMap (fst(n),snd(n)) 9
                            |> List.except newAcc
            loop (newPoints@ns) newAcc
    loop [(i,j)] []

let multipleTopThreeBasinsBySize (heightMap:int32[,]) =
    heightMap
    |> Array2D.mapi(fun i j -> getLowPointi heightMap (i,j))
    |> Seq.cast<(int32 * int32) option>
    |> Seq.choose id
    |> Seq.map(getBasin heightMap)
    |> Seq.map(fun l -> l |> List.length)
    |> Seq.sortByDescending id
    |> Seq.take 3
    |> Seq.reduce(*)

let runDayNine =
    let testLowPoints = readLines "test-day-nine-1.txt"
                        |> List.map(parseDigits)
                        |> toDigitArray
                        |> sumRisksForLowPoints
    printfn "%A" testLowPoints
    
    let probLowPoints = readLines "prob-day-nine-1.txt"
                        |> List.map(parseDigits)
                        |> toDigitArray
                        |> sumRisksForLowPoints
    printfn "%A" probLowPoints
    
    let testBasinSize = readLines "test-day-nine-1.txt"
                        |> List.map(parseDigits)
                        |> toDigitArray
                        |> multipleTopThreeBasinsBySize
    printfn "%i" testBasinSize

    let probBasinSize = readLines "prob-day-nine-1.txt"
                        |> List.map(parseDigits)
                        |> toDigitArray
                        |> multipleTopThreeBasinsBySize
    printfn "%i" probBasinSize
