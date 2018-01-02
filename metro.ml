let _ =
	if Array.length Sys.argv = 2 then
		let filename = Sys.argv.(1) in
		let (l, _) = analyse_file_1 in
		list_to_table l
	else
		Printf.printf "usage: %s filename\n" Sys.argv.(0)
