module fsharp99

let rec myLast ys = 
    match ys with
    | [] -> failwith "No last element in an empty list!"
    | [x] -> x
    | _::xs -> myLast xs
