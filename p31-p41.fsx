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
