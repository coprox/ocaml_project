module type TRAIN =
sig
  val tgv : (string) list
  val euro : (string) list
  val thalys : (string) list

  val get_speed : string -> int
end

module type DATA =
sig
  type train = (string * string * int)

  val tab : train list

  val get_start : train -> string
  val get_finish : train -> string
  val get_length : train -> int
end

module type ENTRY =
sig
  val entry : string list
end

module type TRIP =
sig
  type travel = (string * int * int * string * string * string list)

  val trip : travel

  val get_trip : string -> int -> int -> string -> string -> string list -> travel
  val get_train : travel -> string
  val get_id : travel -> int
  val get_speed : travel -> int
  val get_date : travel -> string
  val get_time : travel -> string
  val get_city : travel -> string list
end

module type MAKETRIP = functor (Train : TRAIN) (Data : DATA) (Entry : ENTRY) -> TRIP

module MakeTrip : MAKETRIP = functor (Train : TRAIN) (Data : DATA) (Entry : ENTRY) ->
struct
  type travel = (string * int * int * string * string * string list)

  let get_train (train, _, _, _, _, _) = train
  let get_id (_, id, _, _, _, _) = id
  let get_speed (_, _, speed, _, _, _) = speed
  let get_date (_, _, _, date, _, _) = date
  let get_time (_, _, _, _, time, _) = time
  let get_city (_, _, _, _, _, city) = city
  let get_trip train id speed date time city = (train, id, speed, date, time, city)

  (* let trip = *)
  (*   let rec create_trip cmp list train id speed date time city = *)
  (*     match cmp with *)
  (* 	| 0 -> create_trip (cmp + 1) (List.tl list) (List.hd list) id speed date time city *)
  (* 	| 1 -> *)
  (* 	  begin *)
  (* 	    Random.init 42; *)
  (* 	    create_trip (cmp + 1) (List.tl list) train ((Random.int 9000) + 1000) speed date time city *)
  (* 	  end *)
  (* 	| 2 -> *)
  (* 	  if ((List.hd list) = "TGV") then *)
  (* 	    create_trip (cmp + 1) (List.tl list) train id (Train.speed_euro) date time city *)
  (* 	  else if  ((List.hd list) = "Thalys") then *)
  (* 	    create_trip (cmp + 1) (List.tl list) train id (Train.speed_thalys) date time city *)
  (* 	  else if  ((List.hd list) = "Eurostar") then *)
  (* 	    create_trip (cmp + 1) (List.tl list) train id (Train.speed_euro) date time city *)
  (* 	| 3 -> create_trip (cmp + 1) (List.tl list) train id speed (List.hd list) time city *)
  (* 	| 4 -> create_trip (cmp + 1) (List.tl list) train id speed date (List.hd list) city *)
  (* 	| 5 -> create_trip (cmp + 1) (List.tl list) train id speed date time (Str.split (Str.regexp ",") (List.hd list)) *)
  (* 	| 6 -> () (\* get_trip train id speed date time city *\) *)
  (* 	| _ -> () *)
  (*   in create_trip 0 (Str.split (Str.regexp "") Entry.entry) "" 0 0 "" "" [""] *)

  let trip =
    begin
      Random.init 42;
      get_trip (List.nth Entry.entry 1) ((Random.int 9000) + 1000) (Train.get_speed (List.nth Entry.entry 1)) (List.nth Entry.entry 2) (List.nth Entry.entry 3) (Str.split (Str.regexp ",") (List.nth Entry.entry 4))
    end
end
