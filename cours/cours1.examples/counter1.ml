(*  Regrouper le code écrit pour un compteur *)
module Counter =
  struct
    let c = ref 0
    let incr () = c:= !c+1
    let show () = !c
  end
;;
