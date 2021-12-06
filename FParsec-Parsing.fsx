#r "nuget: FParsec"
open FParsec

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg;;

let ws = spaces;;
let str s = pstring s;;

let draws = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n";;
let drawParser : Parser<int list, unit> = sepEndBy pint32 (pstring ",") .>> newline;;
test drawParser draws

let board = @"22 13 17 11  0\n
 8  2 23  4 24\n
21  9 14 16  7\n
 6 10  3 18  5\n
 1 12 20 15 19\n";;

let floatBetweenBrackets : Parser<float,unit> = str "[" >>. pfloat .>> str "]";;