module fsharp99

type Tree<'a> = 
    | Empty 
    | Branch of 'a * Tree<'a> * Tree<'a>

(* P55 - Construct completely balanced binary trees *)
let rec cBalancedTrees n =
    match n with
    | 0 -> [Empty]
    | 1 -> [Branch('x', Empty, Empty)]
    | _ when n > 0 -> 
        let n1 = (n - 1)/2
        let n2 = n - n1 - 1
        if n1 <> n2 then
            let tn1 = cBalancedTrees n1
            let tn2 = cBalancedTrees n2
            List.concat [for t' in tn1 do 
                            for t'' in tn2 do
                                yield [Branch('x', t', t''); Branch('x', t'', t')]]
        else
            let tn1 = cBalancedTrees n1
            [for t' in tn1 do
                for t'' in tn1 do
                    yield Branch('x', t', t'')]
    | _ -> failwith "Cannot generate trees with negative number of nodes!"
