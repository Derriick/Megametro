type way
type table

val empty : table
val is_empty : table -> bool
val is_present : string -> table -> bool
val add_way : way -> table -> table
val list_to_table : (string * string * int) list -> table
val print_table : table -> unit
val best_way : string -> string -> table -> int * (string list)
val best_comb_path : string list list -> table -> (string list * int list) list
