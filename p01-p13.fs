module fsharp99

(* P01 - find the last element of a list *)
let rec myLast ys = 
    match ys with
    | [] -> failwith "No last element in an empty list!"
    | [x] -> x
    | _::xs -> myLast xs

(* P02 - find the last but one element of a list *)
let rec myButLast ys =
    match ys with
    | []  -> failwith "No last but one element in an empty list"
    | [x] -> failwith "No last but one element in a list with only one element"
    | [x;_] -> x
    | _::xs -> myButLast xs

(* P03 - find the kth element of a list *)
let rec elementAt k ys = 
    match (k, ys) with 
    | (k, _) when k <= 0 -> failwith "Invalid index requested"
    | (_, []) -> failwith "List is too small"
    | (1, x::xs) -> x
    | (k, x::xs) -> elementAt (k - 1) xs 

(* P04 - find the number of elements in a list *)
let rec myLength xs = 
    match xs with
    | [] -> 0
    | x::xs' -> 1 + (myLength xs')

(* P05 - reverse a given list *)
let myReverse xs =
    let rec myReverse' rs ys =
        match ys with 
        | [] -> rs
        | y::ys' -> myReverse' (y::rs) ys'
    myReverse' [] xs

(* P06 - returns true if a given list is a palindrome, false otherwise *)
let isPalindrome xs =
    let xs' = List.rev xs
    xs = xs'

(* P07 - flattens a nested list structure *)
type NestedList<'a> =
    | Elem of 'a
    | Lst of NestedList<'a> list

let rec flattenNested xs = 
    match xs with
    | Elem(x) -> [x]
    | Lst([]) -> []
    | Lst(y::ys) -> List.concat [flattenNested y; flattenNested (Lst ys)]

(* P08 - Replaces consecutive elements of a list with a single element *)
let compress xs = 
    let rec compress' zs t ys  =
        match ys with 
        | [] -> t::zs 
        | y::ys' -> if (t = y) then (compress' zs t ys') else (compress' (t::zs) y ys')
    match xs with
    | [] -> []
    | x::xs -> List.rev (compress' [] x xs)

(* P09 - Pack consecutive equivalent items of a list into sublists *)
let pack xs = 
    let rec pack' soFarL currL ys = 
        match ys with
        | [] ->  currL::soFarL
        | y::ys' -> let t = List.head currL
                    if (t = y) 
                    then (pack' soFarL (y::currL) ys') 
                    else (pack' (currL::soFarL) [y] ys')
    match xs with 
    | [] -> []
    | x::xs' -> List.rev (pack' [] [x] xs')


(* P10 - Encode a list *)
let encode xs = [for xs' in (pack xs) -> (List.length xs', List.head xs')]

(* P11 - Modified run-length encoding *)
type Coding<'a> = 
    | Single of 'a
    | Multiple of int * 'a

let encodeModified xs = 
    [for xs' in pack xs -> let l = List.length xs'
                           let x = List.head xs'
                           if l = 1 then Single(x) else Multiple(l, x)]

(* P12 - Decode a run-length encoded list *)
let decodeModified xs = 
    List.concat [for xs' in xs -> match xs' with
                                  | Single(x) -> [x]
                                  | Multiple(n, x) -> List.replicate n x]

(* P13 - Directly run-length encode a list *)
let encodeDirect xs = 
    let rec encode' soFar count t xs' = 
        let item n x = if (n = 1) then Single(x) else Multiple(n, x)
        match xs' with
        | [] -> (item count t)::soFar
        | x'::xs'' -> if (t = x') 
                      then encode' soFar (count + 1) t xs''
                      else encode' ((item count t)::soFar) 1 x' xs''
    match xs with
    | [] -> []
    | x::xs' -> List.rev (encode' [] 1 x xs')
                                    
