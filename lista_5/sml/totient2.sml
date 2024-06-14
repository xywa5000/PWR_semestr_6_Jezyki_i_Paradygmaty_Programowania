fun prime_factors n =
    let
        fun factors n d acc =
            if n = 1 then acc
            else if n mod d = 0 then factors (n div d) d (d :: acc)
            else factors n (d + 1) acc
    in
        List.rev (factors n 2 [])
    end;

fun intPow (x, 0) = 1
  | intPow (x, n) = x * intPow (x, n - 1)

fun group [] = []
  | group (x::xs) = 
    let
        val (prefix, suffix) = List.partition (fn y => y = x) xs
    in
        (x::prefix) :: group suffix
    end

fun totient2 n =
    let
        fun product (p, k) = (p - 1) * intPow (p, k - 1)
        val factors = prime_factors n
        val groupedFactors = group factors
        val factorCounts = List.map (fn xs => (List.hd xs, List.length xs)) groupedFactors
    in
        List.foldl (fn (pk, acc) => acc * product pk) 1 factorCounts
    end

fun main () =
    (
        print ("totient2(10) = " ^ Int.toString (totient2 10) ^ "\n");
        print ("totient2(20) = " ^ Int.toString (totient2 20) ^ "\n");
        print ("totient2(50) = " ^ Int.toString (totient2 50) ^ "\n");
        print ("totient2(100) = " ^ Int.toString (totient2 100) ^ "\n")
    );

main ();
