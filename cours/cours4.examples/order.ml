(* l'ordre n'est pas spécifié *)
(print_string "gauche\n"; fun x -> x)
(print_string "droite\n"; 42)
;;

(* forcer un ordre d'évaluation *)
let f = print_string "gauche\n"; fun x -> x
in f (print_string "droite\n"; 42)
