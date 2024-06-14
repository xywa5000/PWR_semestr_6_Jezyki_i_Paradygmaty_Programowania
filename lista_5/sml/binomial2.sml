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

fun main () =
    (
        print ("(binomial2 5 4) = " ^ (Int.toString (binomial2 (5, 4)) ^ "\n"));
        print ("(binomial2 7 3) = " ^ (Int.toString (binomial2 (7, 3)) ^ "\n"));
        print ("(binomial2 9 2) = " ^ (Int.toString (binomial2 (9, 2)) ^ "\n"));
        print ("(binomial2 11 6) = " ^ (Int.toString (binomial2 (11, 6)) ^ "\n"))
    );

main ()
