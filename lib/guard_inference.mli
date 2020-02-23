open Program

module type Guard_inference = sig
  type t
  module M : Manip with type t = t

  val infer_guard : t -> t list -> (t, (t, rbinpred) lit) pre_safe -> (t,rbinpred) guard option
end

module Make(M : Manip) : Guard_inference with type t = M.t
