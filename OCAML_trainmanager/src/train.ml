module type TRAIN =
sig
  val tgv : (string) list
  val euro : (string) list
  val thalys : (string) list

  val get_speed : string -> int
end

module Train : TRAIN =
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
