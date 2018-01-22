module type TRAIN =
sig
  val tgv : (string) list
  val euro : (string) list
  val thalys : (string) list

  val get_speed : string -> int
end

module Train : TRAIN
