module fsharp99

#load "utils.fsx"

(* Returns a list of count no. of random numbers between minVal and maxVal inclusive *)
let genRandomInts1 count minVal maxVal = 
    let r = new System.Random()
    [for i in 1..count do yield r.Next(minVal, maxVal) ]
    
let genRandomInts2 count minVal maxVal =
    let r = new System.Random()
    [for i in 1..count do yield r.Next(minVal, maxVal - i + 1)]

(* P24 - Extracts a n elements from a list randomly *)
let rndSelect (xs:'a list) (n:int) = 
    let rec rndSelect' rands xs n =
        match rands, xs, n with
        | _, [], _ -> []
        | _, _, 0 -> []
        | r::rands', xs, n  -> (elementAt r xs)::(rndSelect' rands' (removeNth r xs) (n-1))
    let rands = genRandomInts2 n 1 (List.length xs)
    rndSelect' rands xs n
