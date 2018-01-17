(**************************************************************)
(* FONCTIONS PERMETTANT D'AFFICHER LES DIFFERENTES STRUCTURES *)
(**************************************************************)

(* fonction auxiliaire *)
let rec print_succs l_succs =
	match l_succs with
	| [] -> ()
	| (s, (d, b))::l_succs' ->
		let _ = Printf.printf "  \\__ %s : %d | %d\n" s d b in
		print_succs l_succs'

(* fonction auxiliaire *)
let rec print_table_aux l =
	match l with
	| [] -> ()
	| (s, s_succs)::l' ->
		let l_succs = MS.bindings s_succs in
		let _ = Printf.printf "%s\n" s in
		let _ = print_succs l_succs in
		print_table_aux l'

(* AFFICHE LA TABLE T *)
let rec print_table t =
	let l = MS.bindings t in
	print_table_aux l

(* affiche l'itinéraire (pp) (avec les temps d'attente) *)
let rec print_pp pp = 
	match pp with
	| [] -> Printf.printf "\n"
	| (s, d)::pp' ->
		let _ = Printf.printf "(%s, %d) " s d in
		print_pp pp' 

(* affiche la liste (ppl) des itinéraires (pp) (avec les temps d'attente) *)
let rec print_ppl ppl =
	match ppl with
	| [] -> Printf.printf "\n"
	| pp::ppl' ->
		let _ = print_pp pp in
		print_ppl ppl'