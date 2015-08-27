module fsharp99

(* P14 - duplicate elements of a list *)
let rec dupli xs = 
    match xs with
    | [] -> []
    | x::xs' -> x::x::(dupli xs')
