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

(* P18 - Extract a slice of ith to kth elements of a list. Counting starts from 1 *)
let slice xs i k = drop (i-1) (take k xs)

(* P19 - Rotate an list n positions to the left *)
let rotate xs n =
    let l = List.length xs
    if n > 0 then
        let n' = n % l
        List.append (drop n' xs) (take n' xs)
    else
        let n' = l - ((abs n) % l)
        List.append (drop n' xs) (take n' xs)

(* P20 - Removes the kth element of a list *)
let removeAt xs k = List.append (take (k-1) xs) (drop k xs)

(* P21 - Insert an element at a given position into a list *)
let insertAt x xs n = List.concat [take (n-1) xs; [x]; drop (n-1) xs]

(* P22 - Create a list with all the elements in a given range *)
let range st en =
    if st < en then [for x = st to en do yield x]
    else [for x = st downto en do yield x]
