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
                    

(* For quickly testing P48 *)
let exp2 xs = 
    match xs with
    | [x; y] -> And x y
    | [x; y; z] -> And (Or x y) (Not z)
    | _ -> failwith "No expression available for that list"

(* P48 - truth tables for expressions in general *)
let tablen n func =
    let rec genInputs n =
        match n with
        | 1 -> [[true];[false]]
        | _ -> let inps' = genInputs (n-1)
               [for inp in inps' do yield true::inp] @ [for inp in inps' do yield false::inp]

    let rec eval inps =
        match inps with
        | inp::inps' ->
            let rec printInp inp' =
                match inp' with
                | t::inp'' -> printf "%A " t
                              printInp inp''
                | [] -> printfn "%A" (func inp)

            printInp inp
            eval inps'

        | [] -> ()

    eval (genInputs n) 

(* P49 - gray code with caching *)
let rec gray n cache =
    if Map.containsKey n cache 
    then (Map.find n cache, cache)
    else 
        let rec gray' n cache =
            match n with
            | 0 -> ([""], cache)
            | _ -> let (subGray, cache') = gray (n-1) cache
                   let newGray = [for s in subGray do yield "0" + s] @ [for s in (List.rev subGray) do yield "1" + s]
                   let cache'' = Map.add n newGray cache'
                   (newGray, cache'')
        gray' n cache
