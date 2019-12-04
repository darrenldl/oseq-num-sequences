module type B = sig
  type t

  val of_int : int -> t

  val succ : t -> t

  val pred : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t
end

module type Integral = sig
  type t

  val nat : t Seq.t

  val range_exc : ?incre:t -> t -> t -> t Seq.t

  val range_inc : ?incre:t -> t -> t -> t Seq.t

  val zero_to_n_exc : t -> t Seq.t

  val zero_to_n_inc : t -> t Seq.t

  val modulo : t -> t Seq.t
end

module Make_integral (B : B) : Integral = struct
  type t = B.t

  let zero = B.of_int 0

  let one = B.of_int 1

  let nat = OSeq.iterate zero B.succ

  let rec range_exc ?(incre = one) start end_exc =
    if start < end_exc then
      OSeq.cons start (range_exc (B.add start incre) end_exc)
    else OSeq.empty

  let range_inc ?(incre = one) start end_inc =
    range_exc ~incre start (B.succ end_inc)

  let zero_to_n_exc n = range_exc zero n

  let zero_to_n_inc n = range_inc zero n

  let modulo n = OSeq.cycle (zero_to_n_exc n)
end

module Int = Make_integral (struct
    include Int

    let of_int x = x
  end)

module Int32 = Make_integral (Int32)
module Int64 = Make_integral (Int64)
