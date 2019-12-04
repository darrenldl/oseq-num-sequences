module type B = sig
  type t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t
end

module type S = sig
end

module Make (B : B) = struct
end
