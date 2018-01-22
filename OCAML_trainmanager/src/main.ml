module type DATA =
sig
  type train = (string * string * int)

  val tab : train list

  val get_start : train -> string
  val get_finish : train -> string
  val get_length : train -> int
end

module D : DATA =
struct
  type train = (string * string * int)

  let tab = [("Paris", "Lyon", 427);
	     ("Dijon", "Lyon", 192);
	     ("Paris", "Lille", 225);
	     ("Paris", "Nancy", 327);
	     ("Dijon", "Nancy", 226);
	     ("Brest", "Rennes", 248);
	     ("Lille", "London", 269);
	     ("Liege", "Cologne", 118);
	     ("Le Mans", "Paris", 201);
	     ("Cologne", "Essen", 81);
	     ("Lyon", "Marseille", 325);
	     ("Brussels", "Liege", 104);
	     ("Paris", "Le Havre", 230);
	     ("Rennes", "Le Mans", 163);
	     ("Le Mans", "Nantes", 183);
	     ("Paris", "Bordeaux", 568);
	     ("Lille", "Brussels", 106);
	     ("Nancy", "Strasbourg", 149);
	     ("Paris", "Strasbourg", 449);
	     ("Dijon", "Strasbourg", 309);
	     ("Toulouse", "Bordeaux", 256);
	     ("Brussels", "Amsterdam", 211);
	     ("Montpellier", "Toulouse", 248);
	     ("Marseille", "Montpellier", 176)]
  let get_start (s, _, _) = s
  let get_finish (_, f, _) = f
  let get_length (_, _, l) = l
end

module type TRAIN =
sig
  val tgv : (string) list
  val euro : (string) list
  val thalys : (string) list

  val get_speed : string -> int
end

module T : TRAIN =
struct
  let tgv = ["Brest";
	     "LeHavre";
	     "Lille";
	     "Paris";
	     "Strasbourg";
	     "Nancy";
	     "Dijon";
	     "Lyon";
	     "Nice";
	     "Marseille";
	     "Montpellier";
	     "Perpignan";
	     "Bordeaux";
	     "Nantes";
	     "Avignon";
	     "Rennes";
	     "Biarritz";
	     "Toulouse";
	     "LeMans"]
  let euro = ["Paris";
	      "London";
	      "Brussels";
	      "Lille"]
  let thalys = ["Paris";
		"Lille";
		"Liege";
		"Brussels";
		"Amsterdam";
		"Cologne";
		"Essen"]

  let get_speed = function
    | "TGV" -> 230
    | "Thalys" -> 210
    | "Eurostar" -> 160
    | _ -> 0
end

module type ENTRY =
sig
  val entry : string list
end

module type TRIP =
sig
  type travel

  val trip : string list -> (string * int * int * string * string * string list)

  val get_trip : string -> int -> int -> string -> string -> string list -> travel
  val get_train : travel -> string
  val get_id : travel -> int
  val get_speed : travel -> int
  val get_date : travel -> string
  val get_time : travel -> string
  val get_city : travel -> string list
end

module type MAKETRIP = functor (Train : TRAIN) (Data : DATA) (Entry : ENTRY) -> TRIP

module M : MAKETRIP = functor (Train : TRAIN) (Data : DATA) (Entry : ENTRY) ->
struct
  type travel = (string * int * int * string * string * string list)

  let get_train (train, _, _, _, _, _) = train
  let get_id (_, id, _, _, _, _) = id
  let get_speed (_, _, speed, _, _, _) = speed
  let get_date (_, _, _, date, _, _) = date
  let get_time (_, _, _, _, time, _) = time
  let get_city (_, _, _, _, _, city) = city
  let get_trip train id speed date time city = (train, id, speed, date, time, city)

  let trip list =
    let a = ((Random.int 9000) + 1000)
    in
    begin
      Printf.printf ("Trip created: %s %d\n") (List.nth list 0) a;
      get_trip (List.nth list 0) a (Train.get_speed (List.nth list 0)) (List.nth list 1) (List.nth list 2) (Str.split (Str.regexp ",") (List.nth list 3))
    end
end

let split_string list = match list with
  | [] -> list
  | _ -> List.append (Str.split (Str.regexp " ") (List.hd list)) (List.tl list)



let rec print_cities trip list date time =
  match trip with
    | ("", 0, 0, "", "", []) -> ()
    | (train, id, vit, new_d, hour, cities) ->
      if (new_d = date && hour = time)
      then
	begin
	  Printf.printf ("%s (,) (%s,%s)\n") (List.hd list) (date) (time);
	  print_cities trip (List.tl list) ("") ("")
	end
      else if (List.tl list = [])
      then
	begin
	  Printf.printf ("%s (%s,%s) (,)\n") (List.hd list) (new_d) (hour);
	end
      else
	begin
	  Printf.printf ("%s (%s,%s) (%s,%s)\n") (List.hd list) (new_d) (hour) (new_d) (hour);
	  print_cities trip (List.tl list) ("") ("")
	end

let print_list list =
  match list with
  | ("", 0, 0, "", "", []) -> ()
  | (train, id, vit, date, time, cities) ->
    begin
      Printf.printf ("%s %d\n") (train) (id);
      print_cities (train, id, vit, date, time, cities) cities date time
    end

let rec main list =
  let str = try read_line ()
    with End_of_file -> exit 0
  in
  if (str = "quit") then (exit 0)
  else
    match (Str.split (Str.regexp  " ") str) with
      | [] -> main list
      | hd::tl ->
	if (hd = "create") then
	  begin
	    let module Allo = M (T) (D) (struct let entry = (hd::tl) end)
	    in
	    match list with
	      | ("", 0, 0, "", "", []) -> main (Allo.trip tl)
	      | _ -> main list
	  end
	else if (hd = "delete") then
	  begin
	    main ("", 0, 0, "", "", [])
	  end
	else if (hd = "list") then
	  begin
	    print_list list;
	    main list
	  end
	else
	  begin
	    main list
	  end;;

main ("", 0, 0, "", "", []);;
