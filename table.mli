type way = string * string
type path = string list
type path_pass = (string * int) list
type table

exception No_way

val list_to_table : (string * string * int) list -> table
val best_path : way -> table -> path * int
val best_comb_path : path list -> table -> (string list * int list) list * int
