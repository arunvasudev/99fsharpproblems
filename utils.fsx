module utils

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

let elementAt n xs = List.head (drop (n-1) xs)

let removeNth n xs = (take (n-1) xs) @ (drop n xs)
