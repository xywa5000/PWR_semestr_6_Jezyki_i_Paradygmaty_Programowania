(* lib_sml.sml *)

(* (1) binomial *)

fun binomial (n, k) =
    if k = 0 then 1
    else if n = 0 then 0
    else n * binomial (n - 1, k - 1) div k;

(* (2) binomial2 *)

fun binomial2 (n, k) =
    let
        val triangle = Array.tabulate (n+1, fn i =>
                          Array.tabulate (i+1, fn j =>
                              if j = 0 orelse j = i then 1
                              else 0))

        fun fillTriangle (i, j) =
            if i > n then ()
            else (
                if j > i - 1 then fillTriangle (i + 1, 1)
                else (
                    Array.update (Array.sub (triangle, i), j,
                                  Array.sub (Array.sub (triangle, i-1), j-1) +
                                  Array.sub (Array.sub (triangle, i-1), j));
                    fillTriangle (i, j + 1)
                )
            )
    in
        fillTriangle (1, 1);
        Array.sub (Array.sub (triangle, n), k)
    end;

(* (3) mergesort *)

fun merge (xs, []) = xs
  | merge ([], ys) = ys
  | merge (x :: xs, y :: ys) =
      if x <= y then x :: merge(xs, y :: ys)
      else y :: merge(x :: xs, ys);

fun mergesort [] = []
  | mergesort [x] = [x]
  | mergesort xs =
      let
        fun split xs =
          let
            fun split' (xs, left, right) =
              case xs of
                [] => (left, right)
              | [x] => (x :: left, right)
              | x1 :: x2 :: xs' => split' (xs', x2 :: left, x1 :: right)
          in
            split' (xs, [], [])
          end;
        val (left, right) = split xs;
      in
        merge(mergesort left, mergesort right)
      end;

(* (4) de *)

fun extendedGCD 0 b = (b, 0, 1)
  | extendedGCD a b =
    let
        val (g, x, y) = extendedGCD (b mod a) a
    in
        (g, y - (b div a) * x, x)
    end
    
fun gcd (a, b) =
    if b = 0 then a
    else gcd (b, a mod b);

fun de a b = 
    let
        val (g, x, y) = extendedGCD a b
    in
        if gcd (a, b) = g then (x, y, g) else (0, 0, 0)
    end

(* (5) prime_factors *)

fun prime_factors n =
    let
        fun factors n d acc =
            if n = 1 then acc
            else if n mod d = 0 then factors (n div d) d (d :: acc)
            else factors n (d + 1) acc
    in
        List.rev (factors n 2 [])
    end

(* (6) totient *)

fun number_sequence (start, end') =
    if start > end' then []
    else start :: number_sequence (start + 1, end');

fun totient n =
    let
        fun is_coprime x = gcd (x, n) = 1
    in
        length (List.filter is_coprime (number_sequence (1, n)))
    end;

(* (7) totient2 *)

fun power (x, 0) = 1
  | power (x, n) = x * power (x, n - 1)

fun group [] = []
  | group (x::xs) = 
    let
        val (prefix, suffix) = List.partition (fn y => y = x) xs
    in
        (x::prefix) :: group suffix
    end

fun totient2 n =
    let
        fun product (p, k) = (p - 1) * power (p, k - 1)
        val factors = prime_factors n
        val groupedFactors = group factors
        val factorCounts = List.map (fn xs => (List.hd xs, List.length xs)) groupedFactors
    in
        List.foldl (fn (pk, acc) => acc * product pk) 1 factorCounts
    end

(* (8) primes *)

fun sieve lst =
    case lst of
        [] => []
      | p :: rest =>
        p :: sieve (List.filter (fn x => x mod p <> 0) rest);

fun primes n =
    if n < 2 then []
    else sieve (List.tabulate (n - 1, fn i => i + 2));

(* (-) utils *)

fun listToString xs = "[" ^ String.concatWith ", " (List.map Int.toString xs) ^ "]"

fun main () =
    (
        print ("binomial(5 4) = " ^ (Int.toString (binomial (5, 4)) ^ "\n"));
        print ("binomial(7 3) = " ^ (Int.toString (binomial (7, 3)) ^ "\n"));
        print ("binomial(11 6) = " ^ (Int.toString (binomial (11, 6)) ^ "\n"));

        print ("binomial2(5 4) = " ^ (Int.toString (binomial2 (5, 4)) ^ "\n"));
        print ("binomial2(7 3) = " ^ (Int.toString (binomial2 (7, 3)) ^ "\n"));
        print ("binomial2(11 6) = " ^ (Int.toString (binomial2 (11, 6)) ^ "\n"));

        print ("mergesort([2, 4, 3, 1]) = " ^ listToString (mergesort [2, 4, 3, 1]) ^ "\n");
        print ("mergesort([6, 8, 2, 4, 7, 3, 5, 1]) = " ^ listToString (mergesort [6, 8, 2, 4, 7, 3, 5, 1]) ^ "\n");
        print ("mergesort([2, 4, 1, 3, 3, 4, 2, 1]) = " ^ listToString (mergesort [2, 4, 1, 3, 3, 4, 2, 1]) ^ "\n");

        print (case (de 35 15) of (x, y, z) => "de(35 15) = (" ^ Int.toString x ^ ", " ^ Int.toString y ^ ", " ^ Int.toString z ^ ")\n");
        print (case (de 123 72) of (x, y, z) => "de(123 72) = (" ^ Int.toString x ^ ", " ^ Int.toString y ^ ", " ^ Int.toString z ^ ")\n");
        print (case (de 441 19) of (x, y, z) => "de(441 19) = (" ^ Int.toString x ^ ", " ^ Int.toString y ^ ", " ^ Int.toString z ^ ")\n");

        print ("prime_factors(100) = " ^ listToString (prime_factors(100)) ^ "\n");
        print ("prime_factors(12345) = " ^ listToString (prime_factors(12345)) ^ "\n");
        print ("prime_factors(122892) = " ^ listToString (prime_factors(122892)) ^ "\n");

        print ("totient(10) = " ^ Int.toString (totient 10) ^ "\n");
        print ("totient(50) = " ^ Int.toString (totient 50) ^ "\n");
        print ("totient(100) = " ^ Int.toString (totient 100) ^ "\n");

        print ("totient2(10) = " ^ Int.toString (totient2 10) ^ "\n");
        print ("totient2(50) = " ^ Int.toString (totient2 50) ^ "\n");
        print ("totient2(100) = " ^ Int.toString (totient2 100) ^ "\n");

        print ("primes(10) = " ^ listToString (primes 10) ^ "\n");
        print ("primes(50) = " ^ listToString (primes 50) ^ "\n");
        print ("primes(100) = " ^ listToString (primes 100) ^ "\n")
    );

main ()