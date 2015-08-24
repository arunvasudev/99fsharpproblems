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
