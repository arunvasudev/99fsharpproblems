module fsharp99

#load "utils.fsx"

open utils

(* Returns a list of count no. of random numbers between minVal and maxVal inclusive *)
let genRandomInts1 count minVal maxVal = 
    let r = new System.Random()
    [for i in 1..count do yield r.Next(minVal, maxVal + 1) ]
    
let genRandomInts2 count minVal maxVal =
    let r = new System.Random()
    [for i in 1..count do yield r.Next(minVal, maxVal - i + 2)]

(* P23 - Extracts a n elements from a list randomly *)
let rndSelect (xs:'a list) (n:int) = 
    let rec rndSelect' rands xs n =
        match rands, xs, n with
        | _, [], _ -> []
        | _, _, 0 -> []
        | [], _, _ -> []
        | r::rands', xs, n  -> (elementAt r xs)::(rndSelect' rands' (removeNth r xs) (n-1))
    let rands = genRandomInts2 n 1 (List.length xs)
    rndSelect' rands xs n

(* P24 - Select n numbers in the range 1 .. m randomly *)
let rndSelectInRange n m = rndSelect [1..m] n

(* P25 - Generate a permutation of a given list *)
let rndPermute xs = rndSelect xs (List.length xs)

(* P26 - Generate all the combinations of k elements from a list *)
let rec combinations n xs = 
    match n, xs with
    | 0, _ -> [[]]
    | _, [] -> []
    | n, x::xs' -> let combs1 = List.map (fun ys -> x::ys) (combinations (n-1) xs')
                   let combs2 = combinations n xs'
                   combs1 @ combs2

let rec diff xs ys = 
    match xs, ys with
    | [], _ -> []
    | x::xs', ys -> if List.exists (fun x' -> x = x') ys then (diff xs' ys) else x::(diff xs' ys)

(* Helper for p27 - partition a list into components of (L - n) and n 
   elements each, where L is the length of the list *)
let onePartition n xs = 
    if n = List.length xs then [[xs]]
    else
        let combs1 = combinations n xs
        List.map (fun ys -> (diff xs ys)::[ys]) combs1

(* P27 - partition a list into disjoin subsets *)
let rec partitions' ns partns =
    match ns with
    | [] -> partns
    | n::ns' -> 
            let rec nextPartition n partns =
                match partns with
                | [] -> []
                | partn::partns' -> 
                        match partn with
                        | [] -> failwith "unexpected empty list found"
                        | firstPart::rest -> 
                                let combs = onePartition n firstPart
                                let newPartns = List.map (fun p -> p @ rest) combs
                                newPartns @ (nextPartition n partns')
            partitions' ns' (nextPartition n partns)

let partitions ns xs = partitions' ns [[xs]]
