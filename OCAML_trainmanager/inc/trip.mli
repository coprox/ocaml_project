module type TRIP =
sig
  type travel

  val get_train : travel -> string
  val get_id : travel -> int
  val get_speed : travel -> int
  val get_date : travel -> string
  val get_time : travel -> string
  val get_city : travel -> string list

  val trip : travel
end
