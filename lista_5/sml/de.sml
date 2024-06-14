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

fun main () =
    
    (
        print (case (de 35 15) of (x, y, z) => "(" ^ Int.toString x ^ ", " ^ Int.toString y ^ ", " ^ Int.toString z ^ ")\n")
    );


main ();