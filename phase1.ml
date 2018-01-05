open Analyse
open Table

let _ =
	if Array.length Sys.argv = 2 then
		let filename = Sys.argv.(1) in
		let (l, (si, sf)) = analyse_file_1 filename in
		let t = list_to_table l in
		let (time, list_s) = best_way si sf t in
		if (time = -1) then
			Printf.printf "No way from %s to %s\n" si sf
			(* output_sol_1 0 [""] *)
		else
			output_sol_1 time list_s
	else
		Printf.printf "usage: %s filename\n" Sys.argv.(0)
