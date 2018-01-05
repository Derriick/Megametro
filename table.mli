type way = string * string * int
type path = string list
type table

val empty : table
val is_empty : table -> bool
val is_present : string -> table -> bool
val add_way : way -> table -> table
val list_to_table : way list -> table
val print_table : table -> unit
val best_path : string -> string -> table -> path * int
val best_comb_path : path list -> table -> (path * int list) list
