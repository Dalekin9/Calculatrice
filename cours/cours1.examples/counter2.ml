module type CounterItfFull =
  sig
    val c : int ref
    val incr : unit -> unit
    val show : unit -> int
  end

module Counter : CounterItfFull =
  struct
    let c = ref 0
    let incr () = c:= !c+1
    let show () = !c
  end;;
