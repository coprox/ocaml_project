module type DATA =
sig
  type train = (string * string * int)

  val tab : train list

  val get_start : train -> string
  val get_finish : train -> string
  val get_length : train -> int
end

module Data : DATA =
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
