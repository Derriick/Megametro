(* @requires décrivant les pré-conditions : c'est-à-dire conditions sur les paramètres pour une bonne utilisation (pas de typage ici), *)
(* @ensures décrivant la propriété vraie à la sortie de la fonction lorsque les pré-conditions sont respectées, le cas échéant avec mention des comportements en cas de succès et en cas d'échec, *)
(* @raises énumérant les exceptions éventuellement levées (et précisant dans quel(s) cas elles le sont). *)

module MS = Map.Make(String)

type way = string * string
type path = string list
type path_pass = (string * int) list
type table = (int * int) MS.t MS.t

exception All_done
exception No_way

(* @requires
   @ensures  une Map identique à (t) contenant une Map dont la clé est (s) (qu'elle soit déjà dedans ou non)
   @raises *)
let add_station s t =
	if MS.mem s t then
		t
	else
		MS.add s MS.empty t

(* @requires
   @ensures  une Map identique à (t) ne contenant aucun élément dont la clé et (s) ou n'ayant aucun successeur
             chaque élément de la Map retournée est une Map n'ayant aucun successeur dont la clé est (s)
   @raises *)
let remove_station s1 t =
	let t' = MS.remove s1 t in
	MS.fold (
			fun e s2_succs t ->
				let s2 = e in
				let s2_succs_without_s1 = MS.remove s1 s2_succs in
				if (MS.is_empty s2_succs_without_s1) then
					MS.remove s2 t	(* si s2 est vide, c'est qu'il n'y avait qu'un chemin entre s1 et s2 *)
				else
					MS.add s2 s2_succs_without_s1 t
		) t' t'

(* @requires 
   @ensures  une Map identique à (t) contenant un élément dont la clé est (s1) (peut déjà exister dans (t))
             cet élément contient une Map contenent l'élément (d, 0) dont la clé est (s2)
   @raises    *)
let add_way_aux s1 s2 d t =
	let t = add_station s1 t in
	let s1_succs =
		try MS.find s1 t with
			Not_found -> MS.empty
	in
	let s1_succs_with_s2 = MS.add s2 (d, 0) s1_succs in
	MS.add s1 s1_succs_with_s2 t

(* @requires way = (s1, s2)
   @ensures  une Map identique à (t) contenant
              - un élément dont la clé est (s1) (peut déjà exister dans (t))
                cet élément contient une Map contenent l'élément (d, 0) dont la clé est (s2)
              - un élément dont la clé est (s2) (peut déjà exister dans (t))
                cet élément contient une Map contenent l'élément (d, 0) dont la clé est (s1)
   @raises    *)
let add_way way d t =
	let (s1, s2) = way in
	let t' = add_way_aux s1 s2 d t in
	add_way_aux s2 s1 d t'

(* @requires 
   @ensures  la table correspondant à la liste des couples de chemins
               Le chemin (s1, s2, d) relie les stations (s1) et (s2) en un temps (d)
               Une table est une Map dont chaque élément a comme clé une chaîne (s1)
               Ces éléments sont une Map dont chaque élément a comme clé un chaîne (s2)
               Ces éléments contiennent le couple (d, bt) avec le temps d'attente (bt = 0)
               Cela est répété en inversant (s1) et (s2)
   @raises    *)
let rec list_to_table l =
	match l with
	| [] -> MS.empty
	| (s1, s2, d)::l' ->
		let t = list_to_table l' in
		let w = (s1, s2) in
		add_way w d t

(* @requires 
   @ensures  le "chemin avec temps de départ" correspondant au chemin (p)
               le temps de départ (time) est initialement à (-1) => signifie qu'on n'est pas encore parti de ce module
   @raises    *)
let path_to_path_pass p =
	List.map (fun s -> (s, -1)) p

(* @requires 
   @ensures  la liste des "chemins avec temps de départ" correspondant à la liste des chemins (pl)
               le temps de départ (time) de chaque chemin est initialement à (-1) => signifie qu'on n'est pas encore parti de ce module
   @raises    *)
let path_list_to_path_pass_list pl =
	List.map (fun p -> path_to_path_pass p) pl

(* @requires s1_succs est une Map contenant un élément dont la clé est (s2)
   @ensures  (time, 0) où (time) est le temps pour aller d'un module s1 à s2
   @raises   time est mis à -1 si le chemin n'existe pas *)
let get_time s2 s1_succs =
	let (d, _) = try MS.find s2 s1_succs with
		Not_found -> (-1, 0)
	in d

(* @requires w = (s1, s2)
             (t) est une Map dont un élément de clé (s1) contient un élément dont la clé est (s2)
   @ensures  (time, 0) où (time) est le temps pour aller d'un module s1 à s2
   @raises   time est mis à -1 si le chemin n'existe pas *)
let get_way_time w t =
	let (s1, s2) = w in
	let s1_succs =
		try MS.find s1 t with
			Not_found -> MS.empty
	in
	get_time s2 s1_succs

(* @requires liste (pp) non vide
             les modules (s) listé dans les listes (pp) listés dans (ppl) sont contenus dans (t)
   @ensures  la somme des temps entre les modules (s) de la liste pp dont les éléments sont des couples (s, time) (time est le temps de départ du module)
   @raises   arrête le programe si (pp) est une vide *)
let rec get_path_pass_time pp t =
	match pp with
	| [] -> assert false
	| _::[] -> 0
	| (s1, _)::pp' ->
		match pp' with
		| [] -> assert false (* ne peut pas arriver *)
		| (s2, _)::_ ->
			let time = get_way_time (s1, s2) t in
			time + get_path_pass_time pp' t

(* @requires les modules (s) listé dans les listes (pp) listés dans (ppl) sont contenus dans (t)
   @ensures  trie la liste (ppl) de listes (pp) en prenant (get_path_pass_time pp t) décroissant, puis avec (List.length) décroissant
   @raises   met les listes (pp) à la fin de la liste retournée *)
let sort_path_path_list ppl t =
	List.sort (
			fun pp1 pp2 ->
				let c = compare (get_path_pass_time pp1 t) (get_path_pass_time pp2 t) in
				if (c = 0) then
					- compare (List.length pp1) (List.length pp2)
				else
					- c
		) ppl

(* @requires w = (s1, s2) -> t doit contenir un élément à la clé (s1), qui lui même doit contenir le couple (d, bt) à la clé (s2)
   @ensures  le temps d'attente avant que le chemin (w) entre deux modules soit libre
   @raises   Not_found *)
let get_busy_time w t =
	let (s1, s2) = w in
	let s1_succs = MS.find s1 t in (* raise Not_found si la station s1 n'existe pas *)
	let (_, bt) = MS.find s2 s1_succs in (* raise Not_found si la station s2 n'existe pas *)
	bt

(* @requires w = (s1, s2) -> t doit contenir un élément à la clé (s1), qui lui même doit contenir le couple (d, bt) à la clé (s2) (et inversement)
   @ensures  change le couple (d, bt) en (d, d + 1) dans (t) aux éléments correspondants
   @raises   Not_found *)
let set_way_busy w t i =
	let (s1, s2) = w in
	let s1_succs = MS.find s1 t in (* raise Not_found si la station n'existe pas *)
	let s2_succs = MS.find s2 t in (* raise Not_found si la station n'existe pas *)
	let d = get_time s2 s1_succs in
	let s1_succs' = MS.add s2 (d, d + i) s1_succs in
	let s2_succs' = MS.add s1 (d, d + i) s2_succs in
	let t' = MS.add s1 s1_succs' t in
	MS.add s2 s2_succs' t'

(* @requires (pp) est une liste non vide
   @ensures  renvoie le premier chemin (w) dont le temps de départ (time) de son module d'arrivée (s2, time) est (-1)
   @raises   arrête le programme *)
let rec get_next_way_in_path_pass pp =
	match pp with
	| [] -> assert false
	| (s1, _)::pp' ->
		match pp' with
		| [] -> (false, ("", ""))
		| (s2, t2)::_ ->
			if (t2 = -1) then
				(true, (s1, s2))
			else
				get_next_way_in_path_pass pp'

(* @requires 
   @ensures  la liste (pp) en mettant à (time) le temps de départ du premier chemin dont il était à (-1)
   @raises    *)
let set_next_way_in_path_pass pp time =
	let rec aux pp1 pp2 =
		match pp1 with
		| [] -> ([], pp2)
		| (s, t)::pp1' ->
			if (t = -1) then
				(pp1', (s, time)::pp2)
			else
				aux pp1' ((s, t)::pp2)
	in
	let (pp1, pp2) = aux pp [] in
	List.rev_append pp2 pp1

(* @requires 
   @ensures  décrémente ou laisse à 0 le temps d'attente (bt) de tous les éléments (d, bt) des successeurs de successeurs de (t)
   @raises    *)
let inc_time_table t =
	MS.map (
		fun s_succs ->
			MS.map (
				fun (d, bt) ->
					if (bt > 0) then
						(d, bt - 1)
					else
						(d, 0)
			) s_succs
	) t

(* @requires liste (pp) non vide
   @ensures  (true) si tous les couples (s, time) de (pp) sont tels que (time > -1)
             (false) sinon
   @raises   arrête le programme *)
let rec is_arrived pp =
	match pp with
	| [] -> assert false
	| (_, t)::[] -> t > -1
	| _::pp' -> is_arrived pp'

(* @requires 
   @ensures  true si tous les couples (s, time) des listes (pp) de (ppl) sont tels que (time > -1)
             (false) sinon
   @raises    *)
let rec all_arrived ppl =
	match ppl with
	| [] -> true
	| pp::ppl' ->
		if (is_arrived pp) then
			all_arrived ppl'
		else
			false

(* @requires w = (s1, s2) -> un itinéraire existe pour aller de (s1) à (s2) à travers le réseau (qui doivent être contenus dans (t))
             (t) est la table (t) de départ sans les éléments correspondant aux chemins par lesquels on est déjà passé
             (t_imin) est le temps minimum trouvé où (-1) si un itinéraire n'a pas encore été trouvé (comme au 1er appel)
             (path_rev) est l'itinéraire suivi pour arriver jusqu'à cette étape ([] au 1er appel)
   @ensures  renvoit le 1er itinéraire trouvé possédant le temps de parcours (t_min) minimal
   @raises   No_way *)
let rec best_path_aux w t time t_min path_rev =
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
		let s_succs = MS.find s t in (* lève l'exception Not_found s'il n'y a pas de successeur *)
		let t' = remove_station s t in (* pour ne pas repasser par s *)
		MS.fold (
			fun s (d, bt) (path_rev1, time1) ->
				let (path_rev2, time2) =
					try best_path_aux (s, sf) t' (time + d) t_min (s::path_rev) with
						Not_found -> ([], -1)
				in
				if (time1 < 0) then
					(path_rev2, time2)
				else if (time2 < 0 || time1 < time2) then
					(path_rev1, time1)
				else
					(path_rev2, time2)
			) s_succs (path_rev, -1)

(* @requires w = (s1, s2) -> un itinéraire existe pour aller de (s1) à (s2) à travers le réseau (qui doivent être contenus dans (t))
   @ensures  renvoit le 1er itinéraire trouvé possédant le temps de parcours minimal
   @raises   No_way *)
let best_path w t =
	let (s, _) = w in
	let (path_rev, time) = best_path_aux w t 0 (-1) [s] in
	if (time == -1) then
		raise No_way
	else
		(List.rev path_rev, time)

(* @requires (ppl) est une liste de listes pp de couples (s, time)
               - (s): nom d'un module
                  -> doit être une clé de (t) et d'au-moins un de ses successeurs
                  -> deux (s) de pp consécutifs ne peuvent pas être identiques
               - (time): temps de départ de ce module (-1 si on ne l'a pas encore quitté)
             (t) est la table correspondant au réseau
             (time) est égal à 0 a 1er appel
   @ensures  une liste de couples (liste de chaînes, liste d'entiers) correspondant 2 à 2 aux modules empruntés et à leur temps de départ
             (pas de temps de départ pour le module d'arrivée)
   @raises   Not_found *)
let rec best_comb_path_aux ppl t time =
	let aux (ppl, t) pp =
		let (b, w) = get_next_way_in_path_pass pp in (* raise All_done si tout est fait *)
		if b then
			let bt = get_busy_time w t in
			if (bt = 0) then
				let pp' = set_next_way_in_path_pass pp time in
				let (b', w') = get_next_way_in_path_pass pp' in
				if b' then
					let bt' = get_busy_time w' t in
					if (bt' = 0) then
						let t' = set_way_busy w' t 0 in
						(pp'::ppl, t')
					else
						(pp::ppl, t)
				else
					(pp::ppl, t)
			else if (bt = 1) then
				let pp' = set_next_way_in_path_pass pp (time + 1) in
				let (b', w') = get_next_way_in_path_pass pp' in
				if b' then
					let t' = set_way_busy w' t 1 in
					(pp'::ppl, t')
				else
					(pp'::ppl, t)
			else
				(pp::ppl, t)
		else
			(pp::ppl, t)
	in
	let (ppl, t) = List.fold_left (
			fun (ppl, t) pp ->
				try aux (ppl, t) pp with
					All_done -> (ppl, t)
		) ([], t) ppl
	in
	if (all_arrived ppl) then (* si tout le monde est arrivé *)
		(ppl, time + 1)
	else
		let t = inc_time_table t in
		best_comb_path_aux (List.rev ppl) t (time + 1)

(* @requires (pp) liste non vide  couples (s, time)
   @ensures  retourne un couple (liste des modules de l'itinéraire, liste des temps de départ)
   @raises   arrête le programme *)
let rec list_gen pp (acc_pp, acc_time) =
	match pp with
	| [] -> assert false
	| (s, time)::[] ->
		(List.rev (s::acc_pp), List.rev acc_time)
	| (s, time)::pp' ->
		list_gen pp' (s::acc_pp, time::acc_time)

(* @requires (ppl) est une liste de listes pp de couples (s, time)
               - (s): nom d'un module
                  -> doit être une clé de (t) et d'au-moins un de ses successeurs
                  -> deux (s) de pp consécutifs ne peuvent pas être identiques
               - (time): temps de départ de ce module (-1 si on ne l'a pas encore quitté)
             (t) est la table correspondant au réseau
   @ensures  une liste de couples (liste de chaînes, liste d'entiers) correspondant 2 à 2 aux modules empruntés et à leur temps de départ
             (pas de temps de départ pour le module d'arrivée)
   @raises   Not_found *)
let best_comb_path pl t =
	let ppl = path_list_to_path_pass_list pl in
	let ppl_sorted = sort_path_path_list ppl t in
	match ppl_sorted with
	| [] -> assert false
	| pp::_ ->
		let t_max = get_path_pass_time pp t in
		let (ppl_sol, time) = best_comb_path_aux ppl_sorted t 0 in
		let _ =
			let c = compare time t_max in
			if (c = 0) then
				Printf.printf "Solution optimale (%d)\n" time
			else if (c > 0) then
				Printf.printf "Peut-être pas la solution la plus optimale (%d: temps du chemin le plus long dépassé de %d)\n" time (time - t_max)
			else
				Printf.printf "Légèrement impossible... (%d)\n" time
		in
		List.rev_map (fun pp -> list_gen pp ([], [])) ppl_sol
