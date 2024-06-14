fun number_sequence (start, end') =
    if start > end' then []
    else start :: number_sequence (start + 1, end');

fun gcd (a, b) =
    if b = 0 then a
    else gcd (b, a mod b);

fun totient n =
    let
        fun is_coprime x = gcd (x, n) = 1
    in
        length (List.filter is_coprime (number_sequence (1, n)))
    end;

fun main () =
    (
        print ("totient(10) = " ^ Int.toString (totient 10) ^ "\n");
        print ("totient(20) = " ^ Int.toString (totient 20) ^ "\n");
        print ("totient(50) = " ^ Int.toString (totient 50) ^ "\n");
        print ("totient(100) = " ^ Int.toString (totient 100) ^ "\n")
    );

main ();
