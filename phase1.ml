open Analyse
open Table

let _ =
	if Array.length Sys.argv = 2 then
		let filename = Sys.argv.(1) in
		let (l, (si, sf)) = analyse_file_1 filename in
		let t = list_to_table l in
		let _ = print_table t in
		let (path, time) = best_path si sf t in
		if (time = -1) then
			Printf.printf "No way from %s to %s\n" si sf
			(* output_sol_1 max_int [""] *)
		else
			output_sol_1 time path
	else
		Printf.printf "usage: %s filename\n" Sys.argv.(0)
