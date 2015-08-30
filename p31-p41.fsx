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
