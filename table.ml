type way = string * string * int

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
		MS.add s MS.empty t

let rec remove_station_aux s1 t l =
	match l with
	| [] -> t
	| (s2, _)::l' ->
		let s2_succs =
			try succs s2 t with
				Not_found -> MS.empty
		in
		let s2_succs_without_s1 = MS.remove s1 s2_succs in
		let t' =
			if (MS.is_empty s2_succs_without_s1) then
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


(* let fold_node f g v0 =
	NodeMap.fold (
		fun n n_succs acc ->
			f n ac
		) g v0
*)

let fold_s f t v0 =
	MS.fold (
		fun s s_succs acc ->
			f s acc
		) t v0

let next_s s_succs =
	if (MS.is_empty s_succs) then
		raise All_done
	else
		MS.choose s_succs

let s_succs s t =
	try succs s t with
		Not_found -> MS.empty
	
let rec best_way_aux s sf t time t_min l =
	let rec best_way_succs s_succs sf t time t_min l =
		if (MS.is_empty s_succs) then
			raise All_done
		else
			let (s, d) = MS.choose s_succs in
			let s_succs' = MS.remove s s_succs in
			let (time1, l1) =
				try best_way_succs s_succs' sf t time t_min l with
					All_done -> (-1, [])
			in
			let (time2, l2) =
				try best_way_aux s sf t (time + d) t_min (s::l) with
					No_way -> (-1, [])
			in
			if (time1 = -1 && time2 = -1) then
				(-1, [])
			else if (time1 = -1) then
				(time2, l2)
			else if (time2 = -1 || (time1 < time2)) then
				(time1, l1)
			else
				(time2, l2)
	in
	if (time > t_min && t_min <> -1) then
		(-1, l) (* le temps déjà accumulé était plus grand que le minimum trouvé *)
	else if (s = sf) then
		(time, l)
	else
		let s_succs = (* on regarde dans tous les successeurs de s *)
			try succs s t with
				Not_found -> raise No_way
		in
		let t' = remove_station s t in (* pour ne pas repasser par s *)
		let (time', l') = best_way_succs s_succs sf t' time t_min l in
		(time', l')

let best_way si sf t =
	let (time, l) = best_way_aux si sf t 0 (-1) [si] in
	(time, List.rev l)

let rec print_succs s_succs =
	if (MS.is_empty s_succs) then
		()
	else
		let (s, d) = MS.choose s_succs in
		let _ = Printf.printf "\t%s -> %d\n" s d in
		let s_succs' = MS.remove s s_succs in
		print_succs s_succs'

let rec print_table t =
	if (MS.is_empty t) then
		()
	else
		let (s, s_succs) = MS.choose t in
		let _ = Printf.printf "%s:" s in
		let _ = print_succs s_succs in
		let t' = MS.remove s t in
		print_table t'
