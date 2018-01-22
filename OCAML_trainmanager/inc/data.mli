module type DATA =
sig
  type train = (string * string * int)

  val tab : train list

  val get_start : train -> string
  val get_finish : train -> string
  val get_length : train -> int
end

module Data : DATA
