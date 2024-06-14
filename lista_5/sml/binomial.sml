fun binomial (n, k) =
    if k = 0 then 1
    else if n = 0 then 0
    else n * binomial (n - 1, k - 1) div k;


fun main () =
    (
        print ("(binomial 5 4) = " ^ (Int.toString (binomial (5, 4)) ^ "\n"));
        print ("(binomial 7 3) = " ^ (Int.toString (binomial (7, 3)) ^ "\n"));
        print ("(binomial 9 2) = " ^ (Int.toString (binomial (9, 2)) ^ "\n"));
        print ("(binomial 11 6) = " ^ (Int.toString (binomial (11, 6)) ^ "\n"))
    );

main ()