module fsharp99

(* P31 - returns true if n is a prime *)
let isPrime n =
    let intSqrt = int (ceil (sqrt (float n)))
    n <> 1 && (n = 2 || not (List.exists (fun d -> n % d = 0) [2..intSqrt]))

(* P32 - returns the GCD of two numbers *)
let rec gcd a b = 
    match b with 
    | 0 -> a
    | _ -> gcd b (a % b)

(* P33 - determines if two integers are coprime *)
let coprime a b = (gcd a b) = 1

(* P34 - Euler's totient function, naive version *)
let totient n = 
    if n = 1 then 1
    else List.fold (fun acc x -> if (coprime n x) then acc + 1 else acc) 0 [1..(n-1)]

(* P35 - determine the prime factors of a given number, naive version *)
let primeFactors n = 
    let rec divs soFar p n = 
        if n = 0 || n % p <> 0 then soFar
        else divs (p::soFar) p (n / p)
    List.concat [for d in [2..(n+1)] do if isPrime(d) then yield divs [] d n ]

let rec takeWhile pred xs =
    match xs with
    | [] -> [] 
    | x::xs' -> if pred x then x::(takeWhile pred xs') else []

let rec dropWhile pred xs =
    match xs with
    | [] -> []
    | x::xs' -> if pred x then dropWhile pred xs' else xs

(* P36 - Prime factors with their multiplicity *)
let primeFactorsMult n =
    let factors = primeFactors n
    let rec helper facs =
        match facs with 
        | [] -> []
        | f::facs' ->
            let fs = takeWhile (fun x -> x = f) facs
            let facs'' = dropWhile (fun x -> x = f) facs
            (f, List.length fs)::(helper facs'')
    helper factors 

(* P37 - Totient function improved *)
let totientImp n =
    let factors = primeFactorsMult n
    let rec product ns =
        match ns with
        | n::ns' -> n*(product ns')
        | [] -> 1
    product [for (p, m) in factors do yield (p-1)*(pown p (m - 1))]

(* P39 - Return a list of primes in the given range *)
let primesR st en = [for n in st..en do if isPrime(n) then yield n]

(* P40 - find two prime numbers that sum to a given even number *)
let goldbach n =
    if (n <= 2 || n % 2 <> 0) 
        then failwith (Printf.sprintf "Invalid input (%d) for goldbach - must be an even number greater than 2" n)

    let rec aux ns = 
        match ns with
        | p::ns' -> if (isPrime p) && (isPrime (n - p)) 
                    then (min p (n-p), max p (n - p))
                    else aux ns'
        | [] -> failwith "Golbach proven wrong?!"

    aux [2..n] 

(* P41B - print the goldbach pairs whose numbers are greater than a given number *)
let goldbachList' st en m =
    let gpairs' = [for n in st..en do if ((n % 2 = 0) && (n > 2)) then yield goldbach n]
    let gpairs = List.filter (fun (n1, n2) -> n1 > m && n2 > m) gpairs'
    let rec aux pairs =
        match pairs with
        | (n1, n2)::pairs' ->  printfn "%d = %d + %d" (n1 + n2) n1 n2
                               aux pairs' 
        | [] -> ()
    aux gpairs

(* P41A - print the goldbach pairs for all even numbers in a given range *)
let goldbachList st en = goldbachList' st en 1

