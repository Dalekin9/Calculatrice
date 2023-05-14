(* l'argument d'abord ... *)
(fun x -> print_string "corps\n"; x + x)
  (print_string "argument\n";35+24);;

(* l'argument est toujours évalué *)
(fun x -> print_string "corps\n"; 0)
  (print_string "argument\n";35+24);;
