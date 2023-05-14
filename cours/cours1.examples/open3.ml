(* open local *)

module A=struct let a=17 let b=42 end

let a = 1
let b = 2;;

a+b;;

A.(a+b);;
(* A ouvert dans l'expression entre ( ) *)

a+b;;
