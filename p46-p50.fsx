module fsharp99

let And x y = x && y
let Or x y = x || y
let Not x = not x

(* Expressions for testing *)
let exp1 = fun a b -> And a (Or a b)

(* P46 - creates a truth table for the given boolean expression *)
let table func =
    let inps = [for i in [true; false] do for j in [true; false] do yield (i,j)]
    let rec eval inps =
        match inps with
        | (a, b)::inps' ->  
            printfn "%A %A %A" a b (func a b)
            eval inps'
        | [] -> ()
    eval inps
                    
