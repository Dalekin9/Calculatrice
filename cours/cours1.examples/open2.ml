(* open local *)

module A=struct let a=17 let b=42 end

let a = 1;;
let b = 2;;

a+b;;

let open A in a+b;;
(* Ce open est restreint à l'expression après in *)

a+b;;
