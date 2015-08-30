module fsharp99

(* P31 - Returns true if n is a prime *)
let isPrime n =
    let intSqrt = int (ceil (sqrt (float n)))
    n = 2 || not (List.exists (fun d -> n % d = 0) [2..intSqrt])
