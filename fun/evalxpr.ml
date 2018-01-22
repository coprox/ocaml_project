type 'a my_list =
  | Item of ('a * 'a my_list)
  | Empty;;

let first_elem list =
  match list with
    | Empty ->
      begin
	prerr_string "error no number available\n";
	exit 84;
      end
    | Item(hd, tl) -> hd

let rec rev list = match list with
  | Empty ->
    begin
      prerr_string "Missing number\n";
      exit 84;
    end
  | Item(hd, tl) -> swap tl (Item(hd, Empty))
and swap list other = match list with
  | Empty -> other
  | Item(hd, tl) -> swap tl (Item(hd, other));;


let rec display_it list_nb =
  match list_nb with
    |  Empty -> ()
    |  Item(hd, tl) ->

      begin
	Printf.printf "list elem  = %d\n" (hd);
	display_it tl;
      end

let rec display_ti list_nb =
  match list_nb with
    |  Empty ->
      begin
	Printf.printf "\n";
	()
      end
    |  Item(hd, tl) ->
      begin
	Printf.printf "%c" (hd);
	display_ti tl;
      end

let add_minus list_nb pos nb_minus =
  let rec add_it list_nb tmp nb_minus pos count =
    match list_nb with
      | Empty ->  (rev tmp)
      | Item(hd, tl) -> if (pos == count) then
	  if (nb_minus mod 2 != 0) then add_it tl (Item((hd * -1),tmp)) nb_minus pos (count + 1)
	  else add_it tl (Item(hd ,tmp)) nb_minus pos (count + 1)
	else add_it tl (Item(hd ,tmp)) nb_minus pos (count + 1)
  in add_it list_nb Empty nb_minus pos 1


let find_nb string pos =
  let rec find_it string pos cmp count=
    if (cmp == pos) then (count + 1)
    else if (string.[cmp] == 'N') then find_it string pos (cmp + 1) (count + 1)
    else find_it string pos (cmp + 1) count
  in find_it string pos 0 0


let plus_minus count =
  if (count mod 2 == 0) then '+'
  else '-'

let add_elem elem list =
  let rec addit elem list new_list =
    match list with
      | Empty -> (rev (Item(elem, new_list)))
      | Item(hd, tl) -> addit elem tl (Item(hd, new_list))
  in addit elem list Empty

let rec pop_letf_bracket stack_main stack_op choose =
  match stack_op with
    | Empty ->
      begin
	prerr_string "Error with parenthesis\n";
	exit (84)
      end
    | Item(hd,tl) -> if (hd == '(') then
	if (choose == true) then stack_main
	else tl
      else pop_letf_bracket (add_elem hd stack_main) tl choose;;

let check_greater current cmp =
  match current with
    | '+' -> if (cmp == '*' || cmp == '/' || cmp == '^' || cmp == 'v') then true
      else false

    | '-' ->if (cmp == '*' || cmp == '/' || cmp == '^' || cmp == 'v') then true
      else false
    | '*' -> if (cmp == '^' || cmp == 'v') then true
      else false
    | '/' ->  if (cmp == '^' || cmp == 'v') then true
      else false
    | '^' -> if (cmp == 'v') then true
      else false
    |_ -> false

let check_equal current cmp =
  match current with
    | '+' -> if (cmp == '+' || cmp == '-') then true
      else false
    | '-' -> if (cmp == '+' || cmp == '-') then true
      else false
    | '*' -> if (cmp == '*' || cmp == '/') then true
      else false
    | '/' -> if (cmp == '*' || cmp == '/') then true
      else false
    | '^' ->  if (cmp == '^') then true
      else false
    | _ -> if (cmp == 'v') then true
      else false

let rec pop_operator stack_main stack_op old_chara chara choose =
  match stack_op with
    | Empty -> if (choose == true) then stack_main
      else Item(chara, stack_op)
    | Item(hd, tl) ->
      if (((check_greater chara hd) == true || ((check_equal chara  hd) && chara != '^')) && hd != '(' ) then pop_operator (add_elem hd stack_main) tl old_chara chara choose
      else
	if (choose == true) then stack_main
	else Item(chara, stack_op)

let rec pop_all stack_main stack_op =
    match stack_op with
      | Empty -> stack_main
      | Item(hd, tl) -> if (hd == ')') then
	  begin
	    prerr_string "error with parenthesis\n";
	    exit 84
	  end
	else pop_all (add_elem hd stack_main) tl

let find_nb_list list_nb count =
  let rec findit list_nb count cmp =
    match list_nb with
      | Empty ->
	begin
	  prerr_string "number overflow";
	  exit 84
	end
      | Item(hd, tl) -> if (cmp == count) then hd
	else findit tl count (cmp + 1)
  in findit list_nb count 0

let power nb other =
  let rec power_nb nb nb_save other cmp =
    if (other == 0) then 1
    else if (cmp == other) then nb
    else power_nb (nb * nb_save) nb_save other (cmp + 1)
  in power_nb nb nb other 1


let calculate op tmp=
  let rec calcul op tmp first cmp =
    match tmp with
      | Empty ->
	begin
	  prerr_string "error miss operator\n";
	  exit 84
	end
      | Item(hd, tl) ->
	if (cmp == 0 && op == 'v') then Item((sqrt hd), tl)
	else if (cmp == 1 && op == '/' && (int_of_float first) == 0) then
	  begin
	    prerr_string "forbidden divizion by 0\n";
	    exit 84
	  end
	else if (cmp == 1) then
	  match op with
	    | '+' -> Item((hd +. first), tl)
	    | '-' -> Item((hd -. first), tl)
	    | '/' -> Item((hd /. first), tl)
	    | '*' -> Item((hd *. first), tl)
	    | '^' -> Item((hd  ** first), tl)
	    | _ -> tl;
	else  calcul op tl hd (cmp + 1)
  in calcul op tmp 0.0 0

let display list list_nb =
  let rec display_npr list list_nb res cmp=
    match list with
      | Empty ->
	begin
	  Printf.printf "%0.*f\n" 2 (first_elem res);
	  ();
	end
      | Item(hd, tl) -> if (hd == 'N') then display_npr tl list_nb (Item((float (find_nb_list list_nb cmp)), res)) (cmp + 1)
	else display_npr tl list_nb (calculate hd res) cmp
  in display_npr list list_nb Empty 0

let algo string list_nb =
  let rec npr string list_nb stack_main stack_op cmp =
    if (cmp == String.length string) then
      begin
	display (pop_all stack_main stack_op) list_nb;
	()
      end
    else
      match string.[cmp] with
	| 'N' -> npr string list_nb (add_elem 'N' stack_main) stack_op (cmp + 1)
	| '(' -> npr string list_nb stack_main (Item('(', stack_op)) (cmp + 1)
	| ')' -> npr string list_nb (pop_letf_bracket stack_main stack_op true) (pop_letf_bracket stack_main stack_op false) (cmp + 1)
	| _ -> if (cmp == 0) then npr string list_nb (pop_operator stack_main stack_op '+' string.[cmp] true) (pop_operator stack_main stack_op '+' string.[cmp] false) (cmp + 1)
	  else npr string list_nb (pop_operator stack_main stack_op string.[cmp - 1] string.[cmp] true) (pop_operator stack_main stack_op string.[cmp - 1] string.[cmp] false) (cmp + 1)
  in npr string list_nb Empty Empty 0

let handle_unary string list_nb =
  let rec unary string new_string list_nb cmp count check_op check_in=
    if (cmp == (String.length string)) then
      begin
	(* Printf.printf "%s\n" (new_string); *)
	algo new_string list_nb;
	()
      end
    else
      match string.[cmp] with
	| '+' ->
	  if (check_op == false && cmp != 0 && (string.[cmp - 1] == 'N' || string.[cmp -1] == ')')) then unary string new_string list_nb (cmp + 1) count true true
	  else if (check_op == false) then unary string new_string list_nb (cmp + 1) count true false
	  else unary string new_string list_nb (cmp + 1) count check_op check_in
	| '-' ->
	  if (check_op == false && cmp != 0 && (string.[cmp - 1] == 'N' || string.[cmp -1] == ')')) then unary string new_string list_nb (cmp + 1) (count + 1) true true
	  else if (check_op == false) then unary string new_string list_nb (cmp + 1) (count + 1) true false
	  else unary string new_string list_nb (cmp + 1) (count + 1) check_op check_in
	| _ ->
	  if (check_op == true && check_in == true) then unary string (new_string ^ ((Char.escaped (plus_minus count)) ^ (Char.escaped string.[cmp]))) list_nb (cmp + 1) 0 false false
	  else if (check_op == true && string.[cmp] == 'N') then unary string (new_string ^ "N") (add_minus list_nb (find_nb string cmp) count) (cmp + 1) 0 false false
	  else if (check_op == true) then unary string (new_string ^ ((Char.escaped (plus_minus count)) ^(Char.escaped string.[cmp]))) list_nb (cmp + 1) 0 false false
	  else unary string (new_string ^ (Char.escaped string.[cmp])) list_nb (cmp + 1) 0 false false;
  in unary string "" list_nb 0 0  false false


let remove_sep string =
  let rec remove_it string new_string cmp =
    if (cmp == (String.length string)) then new_string
    else
      match string.[cmp] with
	| ' ' -> remove_it string new_string (cmp + 1)
	| '\t' -> remove_it string new_string (cmp + 1)
	|  _ -> remove_it string (new_string ^ (Char.escaped string.[cmp])) (cmp + 1)
  in remove_it string "" 0

let find_int string =
  let rec find_it string cmp nb length check new_string list_nb =
    if (cmp == length && check == false) then
      begin
	(* Printf.printf "%s\n"(remove_sep new_string); *)
			      (* display_it (rev list_nb); *)
	handle_unary (remove_sep new_string) (rev list_nb);
      end
    else if (cmp == length && check == true) then
      begin
	(* display_it (rev (Item(nb, list_nb))); *)
	handle_unary (remove_sep (new_string ^ "N")) (rev (Item(nb, list_nb)));
	()
      end
    else if (string.[cmp] >= '0' && string.[cmp] <= '9') then
	find_it string (cmp + 1) (nb * 10 + ((Char.code string.[cmp]) - 48)) length true new_string list_nb
    else
      if (check == true) then
	  find_it string (cmp + 1) 0 length false ((new_string ^ "N") ^ Char.escaped string.[cmp]) (Item(nb, list_nb))
      else find_it string (cmp + 1) nb length false (new_string ^ Char.escaped string.[cmp]) list_nb
  in find_it string 0 0 (String.length string) false "" Empty


let check_chara string =
  let rec chara string cmp =
    if (cmp == (String.length string)) then string
    else if ((string.[cmp] >= '0' && string.[cmp] <= '9') || string.[cmp] == ')' || string.[cmp] == '(' || string.[cmp] == ' ' || string.[cmp] == '+' || string.[cmp]  == '-' || string.[cmp] == '*' || string.[cmp] == '/' || string.[cmp] == '^' || string.[cmp] == 'v' || string.[cmp] == '\t') then chara string (cmp + 1)
    else
      begin
	prerr_string "Invalid character occured\n";
	exit 84;
      end
  in chara string 0

let  main () =
  let n  = Sys.argv.(1) in
  begin
    find_int (check_chara n);
  end

let _ = main ()
