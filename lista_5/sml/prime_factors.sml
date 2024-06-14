fun prime_factors n =
    let
        fun factors n d acc =
            if n = 1 then acc
            else if n mod d = 0 then factors (n div d) d (d :: acc)
            else factors n (d + 1) acc
    in
        List.rev (factors n 2 [])
    end

fun listToString xs = "[" ^ String.concatWith ", " (List.map Int.toString xs) ^ "]"

fun main () =
    (
        print ("prime_factors(10) = " ^ listToString (prime_factors(10)) ^ "\n");
        print ("prime_factors(50) = " ^ listToString (prime_factors(50)) ^ "\n");
        print ("prime_factors(100) = " ^ listToString (prime_factors(100)) ^ "\n")
    );

main ();
