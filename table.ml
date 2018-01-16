(* @requires décrivant les pré-conditions : c'est-à-dire conditions sur les paramètres pour une bonne utilisation (pas de typage ici), *)
(* @ensures décrivant la propriété vraie à la sortie de la fonction lorsque les pré-conditions sont respectées, le cas échéant avec mention des comportements en cas de succès et en cas d'échec, *)
(* @raises énumérant les exceptions éventuellement levées (et précisant dans quel(s) cas elles le sont). *)

type way = string * string
type path = string list
type path_pass = (string * int) list

module MS = Map.Make(String)
type table = (int * int) MS.t MS.t

exception All_done
exception No_way

let empty = MS.empty

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let is_empty t = MS.is_empty t

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let is_present s t = MS.mem s t

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let succs s t = MS.find s t

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let add_station s t =
	if is_present s t then
		t
	else
		MS.add s empty t

(* @requires  *)
(* @ensures   *)
(* @raises    *)
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

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let remove_station s t =
	let t' = MS.remove s t in
	let l = MS.bindings t' in
	remove_station_aux s t' l

(* @requires  *)
(* @ensures   *)
(* @raises    *)
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

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let add_way way d t =
	let (s1, s2) = way in
	let t2 = add_way_aux s1 s2 d t in
	add_way_aux s2 s1 d t2

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let rec list_to_table_aux l acc =
	match l with
	| [] -> acc
	| (s1, s2, d)::l' ->
		let w = (s1, s2) in
		let t = add_way w d acc in
		list_to_table_aux l' t

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let list_to_table l =
	list_to_table_aux l empty

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let path_to_path_pass p =
	List.map (fun s -> (s, -1)) p

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let path_list_to_path_pass_list pl =
	List.map (fun p -> path_to_path_pass p) pl

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let get_time s2 s1_succs =
	let (d, _) = try MS.find s2 s1_succs with
		Not_found -> (-1, 0)
	in
	d

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let get_way_time w t =
	let (s1, s2) = w in
	let s1_succs =
		try succs s1 t with
			Not_found -> empty
	in
	get_time s2 s1_succs

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let set_way_busy w t =
	let (s1, s2) = w in
	let s1_succs = succs s1 t in (* raise Not_found si la station n'existe pas *)
	let s2_succs = succs s2 t in (* raise Not_found si la station n'existe pas *)
	let d = get_time s2 s1_succs in
	let s1_succs' = MS.add s2 (d, d) s1_succs in
	let s2_succs' = MS.add s1 (d, d) s2_succs in
	let t' = MS.add s1 s1_succs' t in
	MS.add s2 s2_succs' t'

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let get_busy_time w t =
	let (s1, s2) = w in
	let s1_succs = succs s1 t in (* raise Not_found si la station n'existe pas *)
	let (_, b) = MS.find s2 s1_succs in (* raise Not_found si la station n'existe pas *)
	b

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let is_way_busy w t =
	let (s1, s2) = w in
	(get_busy_time s1 s2 t) <> 0

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let rec get_next_way_in_path_pass pp =
	match pp with
	| [] -> assert false
	| (s1, _)::pp' ->
		match pp' with
		| [] -> raise All_done
		| (s2, t2)::_ ->
			if (t2 = -1) then
				(s1, s2)
			else
				get_next_way_in_path_pass pp'


(* @requires  *)
(* @ensures   *)
(* @raises    *)
let set_next_way_in_path_pass pp time =
	let rec aux pp1 pp2 =
		match pp1 with
		| [] -> ([], pp2)
		| (s, t)::pp1' ->
			let pp2' = (s, time)::pp2 in
			if (t = -1) then
				(pp1', pp2')
			else
				aux pp1' pp2'
	in
	let (pp1, pp2) = aux pp [] in
	List.rev_append pp1 pp2

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let inc_time_table t =
	MS.map (
		fun s_succs ->
			MS.map (
				fun (d, b) ->
					if (b > 0) then
						(d, b - 1)
					else
						(d, 0)
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

(* @requires  *)
(* @ensures   *)
(* @raises    *)
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

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let rec print_succs l_succs =
	match l_succs with
	| [] -> ()
	| (s, (d, b))::l_succs' ->
		let _ = Printf.printf "  \\__ %s : %d | %d\n" s d b in
		print_succs l_succs'

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let rec print_table_aux l =
	match l with
	| [] -> ()
	| (s, s_succs)::l' ->
		let l_succs = MS.bindings s_succs in
		let _ = Printf.printf "%s\n" s in
		let _ = print_succs l_succs in
		print_table_aux l'

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let rec print_table t =
	let l = MS.bindings t in
	print_table_aux l
	
(* @requires  *)
(* @ensures   *)
(* @raises    *)
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

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let best_path w t =
	let (s, _) = w in
	let (path_rev, time) = best_path_aux w t 0 (-1) [s] in
	if (time == -1) then
		raise No_way
	else
		(List.rev path_rev, time)

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let sort_path_path_list ppl t =
	List.sort (
			fun p1 p2 ->
				- compare (get_path_pass_time p1 t) (get_path_pass_time p2 t)
		) ppl

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let rec best_comb_path_aux ppl t time =
	let (ppl', t') = List.fold_left (
			fun (ppl, t) pp ->
				let w = get_next_way_in_path_pass pp in
				let b = get_busy_time w t in
				if (b = 0) then
					(* mettre à jour temps de départ de w dans pp de ppl avec time*)
					let ppl' = set_next_way_in_path_pass ppl time in
					let t' = set_way_busy w t in
					(ppl', t')
				else
					(ppl, t)
		) (ppl, t) ppl
	in
	(* tester si tout le monde n'est pas arrivé (à partir des pp de ppl), sinon renvoyer *)
	let t'' = inc_time_table t in
	if (true) then (* si tout le monde n'est pas arrivé *)
		best_comb_path_aux ppl' t'' (time + 1)
	else
		(ppl', time)

(* @requires  *)
(* @ensures   *)
(* @raises    *)
let best_comb_path pl t =
	let ppl = path_list_to_path_pass_list pl in
	let ppl_sorted = sort_path_path_list ppl t in
	match ppl_sorted with
	| [] -> assert false
	| pp::_ ->
		(* let t_max = get_path_pass_time pp t in *)
		let (ppl', time) = best_comb_path_aux ppl_sorted t 0 in
		(* récupérer temps max de ppl (pour chaque pp) et comparer avec t_max -> savoir si c'est optimal ou non *)
		ppl'
