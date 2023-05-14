type pair = P of int*int
let p = P (1,2);;

type pair = P of int*int
let c = P (3,4);;

p = c;; (* types différents *)

type duo = pair;;
let d = (P (5,6):duo);; (* contrainte de type *)

c = d;; (* même type *)
