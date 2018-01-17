open Analyse
open Table

let _ =
	if Array.length Sys.argv = 2 then
		let filename = Sys.argv.(1) in
		let (l, (si, sf)) = analyse_file_1 filename in
		let t = list_to_table l in
		let (path, time) =
			try best_path (si, sf) t with
				No_way ->
					let _ = Printf.printf "No way from %s to %s\n" si sf in
					(* ([si; sf], max_int) *)
					raise No_way
		in
		output_sol_1 time path
	else
		Printf.printf "usage: %s filename\n" Sys.argv.(0)
