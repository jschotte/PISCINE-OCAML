type t = Value.t list

val newMap : unit -> Value.t list

val printMap : Value.t list -> unit

val isWin : Value.t list -> Value.t

val printline : Value.t list -> int -> string

val replace_in_map : int -> int -> Value.t -> Value.t list -> Value.t list

val verif_case : int -> Value.t list -> bool

val verif_draw : Value.t list -> int

val verificationIA : Value.t list -> int
