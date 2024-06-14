fun sieve lst =
    case lst of
        [] => []
      | p :: rest =>
        p :: sieve (List.filter (fn x => x mod p <> 0) rest);

fun primes n =
    if n < 2 then []
    else sieve (List.tabulate (n - 1, fn i => i + 2));

fun listToString xs = "[" ^ String.concatWith ", " (List.map Int.toString xs) ^ "]"

fun main () =
    (
        print ("primes(10) = " ^ listToString (primes 10) ^ "\n");
        print ("primes(50) = " ^ listToString (primes 50) ^ "\n");
        print ("primes(100) = " ^ listToString (primes 100) ^ "\n")
    );

main ();