open Analyse
open Table

let _ =
	if Array.length Sys.argv = 2 then
		let filename = Sys.argv.(1) in
		let (l, path_list) = analyse_file_2 filename in
		let t = list_to_table l in
		let _ = print_table t in
		let list_sol = best_comb_path path_list t in
		output_sol_2 list_sol
	else
		Printf.printf "usage: %s filename\n" Sys.argv.(0)
