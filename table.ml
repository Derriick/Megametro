type way = string * string
type path = string list
type path_pass = (string * int) list

module MS = Map.Make(String)
type table = (int * int) MS.t MS.t

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
	let s1_succs_with_s2 = MS.add s2 (d, 0) s1_succs in
	MS.add s1 s1_succs_with_s2 t

let add_way way d t =
	let (s1, s2) = way in
	let t2 = add_way_aux s1 s2 d t in
	add_way_aux s2 s1 d t2

let rec list_to_table_aux l acc =
	match l with
	| [] -> acc
	| (s1, s2, d)::l' ->
		let w = (s1, s2) in
		let t = add_way w d acc in
		list_to_table_aux l' t

let list_to_table l =
	list_to_table_aux l empty

let path_to_path_pass p =
	List.map (fun s -> (s, -1)) p

let path_list_to_path_pass_list pl =
	List.map (fun p -> path_to_path_pass p) pl

let get_time s2 s1_succs =
	let (d, _) = try MS.find s2 s1_succs with
		Not_found -> (-1, 0)
	in
	d

let get_way_time w t =
	let (s1, s2) = w in
	let s1_succs =
		try succs s1 t with
			Not_found -> empty
	in
	get_time s2 s1_succs

let set_way_busy w t =
	let (s1, s2) = w in
	let s1_succs = succs s1 t in (* raise Not_found si la station n'existe pas *)
	let s2_succs = succs s2 t in (* raise Not_found si la station n'existe pas *)
	let d = get_time s2 s1_succs in
	let s1_succs' = MS.add s2 (d, d) s1_succs in
	let s2_succs' = MS.add s1 (d, d) s2_succs in
	let t' = MS.add s1 s1_succs' t in
	MS.add s2 s2_succs' t'

let get_busy_time w t =
	let (s1, s2) = w in
	let s1_succs = succs s1 t in (* raise Not_found si la station n'existe pas *)
	let (_, b) = MS.find s2 s1_succs in (* raise Not_found si la station n'existe pas *)
	b

let is_way_busy w t =
	let (s1, s2) = w in
	(get_busy_time s1 s2 t) <> 0

let next_way_in_path_pass pp =
	match pp with
	| [] -> assert false
	| _::[] -> raise All_done
	| s1::s2::pp' ->
		(s1, s2)

let inc_time_table t =
	MS.map (
		fun s_succs ->
			MS.map (
				fun (d, b) ->
					if (b >= 0) then
						(d, b - 1)
					else
						(d, b)
			) s_succs
	) t

(*
let rec get_path_pass_time_aux pp t s =
	match pp with
	| [] -> 0
	| (s', _)::pp' ->
		let wtime = get_way_time (s, s') t in
		let ptime = get_path_pass_time_aux pp' t s' in
		wtime + ptime

let get_path_pass_time pp t =
	match pp with
	| [] -> assert false
	| (s, _)::pp' -> get_path_pass_time_aux pp' t s
*)

let rec get_path_pass_time pp t =
	match pp with
	| [] -> assert false
	| _::[] -> 0
	| (s1, _)::pp' ->
		match pp' with
		| [] -> assert false
		| (s2, _)::_ ->
			let time = get_way_time (s1, s2) t in
			time + get_path_pass_time pp' t

let rec print_succs l_succs =
	match l_succs with
	| [] -> ()
	| (s, (d, b))::l_succs' ->
		let _ = Printf.printf "  \\__ %s : %d | %d\n" s d b in
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
	
let rec best_path_aux w t time t_min path_rev =
	let rec best_path_succs l_succs sf t time t_min path_rev =
		match l_succs with
		| [] -> raise All_done
		| (s, (d, _))::l_succs' ->
			let (path_rev1, time1) =
				try best_path_succs l_succs' sf t time t_min path_rev with
					All_done -> ([], -1)
			in
			let (path_rev2, time2) =
				try best_path_aux (s, sf) t (time + d) t_min (s::path_rev) with
					Not_found -> ([], -1)
			in
			if (time1 < 0) then
				(path_rev2, time2)
			else if (time2 < 0 || time1 < time2) then
				(path_rev1, time1)
			else
				(path_rev2, time2)
	in
	let (s, sf) = w in
	if (time > t_min && t_min >= 0) then
		(* le temps déjà accumulé était plus grand que le minimum trouvé *)
		([], -1)
	else if (String.compare s sf = 0) then
		(* la station atteinte est la station finale *)
		(* le temps pour l'atteindre time devient le temps minimal t_min *)
		(path_rev, time)
	else
		(* on regarde dans tous les successeurs de s *)
		let s_succs = succs s t in (* lève l'exception Not_found s'il n'y a pas de successeur *)
		let l_succs = MS.bindings s_succs in
		let t' = remove_station s t in (* pour ne pas repasser par s *)
		best_path_succs l_succs sf t' time t_min path_rev

let best_path w t =
	let (s, _) = w in
	let (path_rev, time) = best_path_aux w t 0 (-1) [s] in
	if (time == -1) then
		raise No_way
	else
		(List.rev path_rev, time)

let sort_path_path_list ppl t =
	List.sort (
			fun p1 p2 ->
				- compare (get_path_pass_time p1 t) (get_path_pass_time p2 t)
		) ppl

let rec best_comb_path_aux ppl t =
	match ppl with
	| [] -> raise All_done
	| pp::ppl' ->
		let t_max = get_path_pass_time pp t in
		assert false

let best_comb_path pl t =
	let ppl = path_list_to_path_pass_list pl in
	let ppl' = sort_path_path_list ppl t in
	best_comb_path_aux ppl' t
