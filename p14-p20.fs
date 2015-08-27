module fsharp99

(* P14 - duplicate elements of a list *)
let rec dupli xs = 
    match xs with
    | [] -> []
    | x::xs' -> x::x::(dupli xs')

(* P15 - Replicate elements of a list a given number of times *)
let repli n xs = 
    let rec repli' n xs = 
        match xs with
        | [] -> []
        | x::xs' -> (List.replicate n x)::(repli' n xs')
    List.concat (repli' n xs)

(* P16 - Drop every nth element of a list *)
let rec drop n xs =
    match (n, xs) with
    | (n, _) when n <= 0 -> xs
    | (_, []) -> []
    | (_, x::xs') -> drop (n-1) xs'

let rec take n xs =
    match (n, xs) with
    | (n, _) when n <= 0 -> []
    | (_, []) -> []
    | (n, x::xs') -> x::(take (n-1) xs')

let dropNth n xs = 
    let rec dropNth' n xs =
        match xs with 
        | [] -> []
        | _ -> (take (n - 1) xs)::(dropNth' n (drop n xs))
    List.concat (dropNth' n xs)

(* P17 - Split a given list after the nth element *)
let split xs n = if n <= 0 then ([], xs) else (take n xs, drop n xs)
