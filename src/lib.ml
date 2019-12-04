module type B = sig
  type t

  val of_int : int -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t
end

module type S = sig
  type t

  val nat : t Seq.t

  val range_exc : ?incre:t -> t -> t -> t Seq.t
end

module Make (B : B) : S = struct
  type t = B.t

  let zero = B.of_int 0

  let one = B.of_int 1

  let pred x = B.(sub x one)

  let succ x = B.(add x one)

  let nat = OSeq.iterate zero succ

  let rec range_exc ?(incre = one) start end_exc =
    if start < end_exc then
      OSeq.cons start (range_exc (B.add start incre) end_exc)
    else OSeq.empty

  let range_inc start end_inc = range_exc start (succ end_inc)

  let zero_to_n_exc n = range_exc zero n

  let zero_to_n_inc n = range_inc zero n

  let modulo n = OSeq.cycle (zero_to_n_exc n)
end

module Int = Make (struct include Int let of_int x = x end)

module Int32 = Make(Int32)

module Int64 = Make(Int64)

module Float = Make(Float)
