module type t = sig
  val satisfiable : Bexp.t -> bool
end