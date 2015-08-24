module fsharp99

let rec myLast ys = 
    match ys with
    | [] -> failwith "No last element in an empty list!"
    | [x] -> x
    | _::xs -> myLast xs

let rec myButLast ys =
    match ys with
    | []  -> failwith "No last but one element in an empty list"
    | [x] -> failwith "No last but one element in a list with only one element"
    | [x;_] -> x
    | x::xs -> myButLast xs
