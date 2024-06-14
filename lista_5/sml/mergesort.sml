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

fun listToString xs = "[" ^ String.concatWith ", " (List.map Int.toString xs) ^ "]"

fun main () =
    (
        print ("mergesort([2, 4, 3, 1]) = " ^ listToString (mergesort [2, 4, 3, 1]) ^ "\n");
        print ("mergesort([6, 8, 2, 4, 7, 3, 5, 1]) = " ^ listToString (mergesort [6, 8, 2, 4, 7, 3, 5, 1]) ^ "\n");
        print ("mergesort([2, 4, 1, 3, 3, 4, 2, 1]) = " ^ listToString (mergesort [2, 4, 1, 3, 3, 4, 2, 1]) ^ "\n")
    );

main ();
