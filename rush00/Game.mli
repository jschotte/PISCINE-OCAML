type t = { map : Map.t list; }

val getMap : t -> Map.t list

val newGame : unit -> Value.t list list

val printEndMap : Value.t list list -> unit

val printGame : Value.t list list -> unit

val replace_in_map :  int -> int -> Value.t -> Value.t list list -> Value.t list list

val check : int -> Value.t list list -> Value.t

val verif_input : int -> int -> Value.t list list -> bool

val verif_draw : Value.t list list -> bool

val getFirstMapFree : Value.t list list -> int
