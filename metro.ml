open Analyse
open Table

let _ =
	if Array.length Sys.argv = 2 then
		let filename = Sys.argv.(1) in
		let (l, (s1, s2)) = analyse_file_1 filename in
		let t = list_to_table l in
		let (time, list_s) = minimal_time s1 s2 t in
		if (time = -1) then
			Printf.printf "No way from %s to %s\n" s1 s2
		else
			(* Printf.printf "%d : %s" time (
				try String.concat " -> " list_s with
					_ ->
						"the result is longer than " ^ string_of_int Sys.max_string_length ^ " bytes"
			) *)
			output_sol_1 time list_s
	else
		Printf.printf "usage: %s filename\n" Sys.argv.(0)
