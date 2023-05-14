(* signature pour des types ordonnés *)
module type Comparable = sig
 type elt
 val compare : elt -> elt -> int
end;;

(* module paramétrique *)
module OrdStack  = functor (T:Comparable) ->
(struct
  exception Empty
  type elt = T.elt
  type t = elt list
  let rec push x = function [] -> x::[]
    | h::t as l when T.compare x h < 0 -> x::l
    | h::t -> h::push x t
  let pop = function [] -> raise Empty
    | h::t -> (h,t)
  let is_empty s = s = []
  let empty () = []
end: Stack with type elt = T.elt);;
