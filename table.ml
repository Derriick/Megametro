type way = string * string * int
type path = string list

module MS = Map.Make(String)
type table = int MS.t MS.t

exception All_done
exception No_way

let empty = MS.empty

let is_empty t = MS.is_empty t

let is_present s t = MS.mem s t

let succs s t = MS.find s t

let add_station s t =
	if is_present s t then
		t
	else
		MS.add s empty t

let rec remove_station_aux s1 t l =
	match l with
	| [] -> t
	| (s2, _)::l' ->
		let s2_succs =
			try succs s2 t with
				Not_found -> empty
		in
		let s2_succs_without_s1 = MS.remove s1 s2_succs in
		let t' =
			if (is_empty s2_succs_without_s1) then
				MS.remove s2 t	(* si s2 est vide, c'est qu'il n'y avait qu'un chemin entre s1 et s2 *)
			else
				MS.add s2 s2_succs_without_s1 t
		in
		remove_station_aux s1 t' l'

let remove_station s t =
	let t' = MS.remove s t in
	let l = MS.bindings t' in
	remove_station_aux s t' l

let add_way_aux s1 s2 d t =
	let t =
		if not (is_present s1 t) then
			add_station s1 t
		else
			t
	in
	let s1_succs =
		try succs s1 t with
			Not_found -> empty
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
	list_to_table_aux l empty

let get_time s2 s1_succs =
	try MS.find s2 s1_succs with
		Not_found -> -1

let get_way_time s1 s2 t =
	let s1_succs =
		try succs s1 t with
			Not_found -> empty
	in
	get_time s2 s1_succs

let rec print_succs l_succs =
	match l_succs with
	| [] -> ()
	| (s, time)::l_succs' ->
		let _ = Printf.printf "  \\__ %s : %d\n" s time in
		print_succs l_succs'

let rec print_table_aux l =
	match l with
	| [] -> ()
	| (s, s_succs)::l' ->
		let l_succs = MS.bindings s_succs in
		let _ = Printf.printf "%s\n" s in
		let _ = print_succs l_succs in
		print_table_aux l'

let rec print_table t =
	let l = MS.bindings t in
	print_table_aux l
	
let rec best_path_aux s sf t time t_min path_rev =
	let rec best_path_succs l_succs sf t time t_min path_rev =
		match l_succs with
		| [] -> raise All_done
		| (s, d)::l_succs' ->
			let (path_rev1, time1) =
				try best_path_succs l_succs' sf t time t_min path_rev with
					All_done -> ([], -1)
			in
			let (path_rev2, time2) =
				try best_path_aux s sf t (time + d) t_min (s::path_rev) with
					Not_found -> ([], -1)
			in
			if (time1 = -1) then
				(path_rev2, time2)
			else if (time2 = -1 || time1 < time2) then
				(path_rev1, time1)
			else
				(path_rev2, time2)
	in
	if (time > t_min && t_min <> -1) then
		(* le temps déjà accumulé était plus grand que le minimum trouvé *)
		([], -1)
	else if (s = sf) then
		(* la station atteinte est la station finale *)
		(* le temps pour l'atteindre time devient le temps minimal t_min *)
		(path_rev, time)
	else
		(* on regarde dans tous les successeurs de s *)
		let s_succs = succs s t in (* lève l'exception Not_found s'il n'y a pas de successeur *)
		let l_succs = MS.bindings s_succs in
		let t' = remove_station s t in (* pour ne pas repasser par s *)
		best_path_succs l_succs sf t' time t_min path_rev

let best_path si sf t =
	let (path_rev, time) = best_path_aux si sf t 0 (-1) [si] in
	(List.rev path_rev, time)

let best_comb_path path_list t =
	assert false
