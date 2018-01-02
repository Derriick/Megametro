type way = string * string * int

module MS = Map.Make(String)
type table = int MS.t MS.t

let empty = MS.empty

let is_empty t = MS.is_empty t

let is_present s t = MS.mem s t

let succs s t = MS.find s t

let add_station s t =
	if is_present s t then
		t
	else
		MS.add s MS.empty t

let add_way_aux s1 s2 d t =
	let t =
		if not (is_present s1 t) then
			add_station s1 t
		else
			t
	in
	let s1_succs =
		try succs s1 t with
			Not_found -> MS.empty
	in
	let s1_succs_with_s2 = MS.add s2 d s1_succs in
	MS.add s1 s1_succs_with_s2 t

let add_way way t =
	let (s1, s2, d) = way in
	let t2 = add_way_aux s1 s2 d t in
	add_way_aux s2 s1 d t2

let rec list_to_table_aux l acc =
	match l with
	| [] -> acc
	| way::l' ->
		let t = add_way way acc in
		list_to_table_aux l' t

let list_to_table l =
	list_to_table_aux l MS.empty

(* vérfier si le temps d'un chemin est déjà supérieur au minimum trouvé *)
let rec minimal_time_aux s2 s1_succs time list_s =
	(-1, [""]) (* A FAIRE !!!!!!!!!!!!! *)

let minimal_time s1 s2 t =
	let s1_succs =
		try succs s1 t with
			Not_found -> MS.empty
	in
	minimal_time_aux s2 s1_succs (-1) []
