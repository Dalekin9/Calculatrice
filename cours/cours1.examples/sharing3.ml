module Mwrite = (M:Write) ;;
module Mread = (M:Read) ;;

let counter = Mwrite.create();;
Mwrite.step counter;;

Mread.get counter;;

(* Mwrite.t est un type différent de Mread.t *)
