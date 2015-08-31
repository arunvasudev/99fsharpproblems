module fsharp99

(* P31 - returns true if n is a prime *)
let isPrime n =
    let intSqrt = int (ceil (sqrt (float n)))
    n = 2 || not (List.exists (fun d -> n % d = 0) [2..intSqrt])

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

(* P37 - Prime factors with their multiplicity *)
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
