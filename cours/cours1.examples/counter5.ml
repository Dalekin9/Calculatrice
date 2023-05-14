let a = MultiCounter.create();;
MultiCounter.incr a;;
MultiCounter.show a;;

(* Interdit (erreur de typage) : *)
a := !a+1;;
