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

(* Some trees for testing *)
let t1 = Branch('x', Branch('x', Empty, Empty), Branch('x', Empty, Empty))
let t2 = Branch('x', Empty, Branch('x', Empty, Empty))

(* Given a tree, returns a 'flipped' version of it *)
let rec flipped t =
    match t with
    | Empty -> Empty
    | Branch(v, t1, t2) -> Branch(v, (flipped t2), (flipped t1))

let rec areIsomorphic t1 t2 =
    match (t1, t2) with
    | (Empty, Empty) -> true
    | (Branch(_, t11, t12), Branch(_, t21, t22)) ->
            (areIsomorphic t11 t21) && (areIsomorphic t12 t22)
    | _ -> false

(*P56 - checks whether a tree is the flipped version of another tree *)
let isSymmetric1 t1 =
    let t1' = flipped t1
    areIsomorphic t1 t1'

(*P56 - a second version that does not construct an intermediate tree *)
let rec areMirrors t1 t2 =
    match (t1, t2) with
    | (Empty, Empty) -> true
    | (Branch(_, t11, t12), Branch(_, t21, t22)) ->
            (areMirrors t11 t22) && (areMirrors t12 t21)
    | _ -> false

let isSymmetric2 t1 = areMirrors t1 t1
