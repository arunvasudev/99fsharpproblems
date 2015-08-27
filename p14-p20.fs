module fsharp99

(* P14 - duplicate elements of a list *)
let rec dupli xs = 
    match xs with
    | [] -> []
    | x::xs' -> x::x::(dupli xs')

(* p15 - Replicate elements of a list a given number of times *)
let rec repli n xs = 
    let rec repli' n xs = 
        match xs with
        | [] -> []
        | x::xs' -> (List.replicate n x)::(repli' n xs')
    List.concat (repli' n xs)
