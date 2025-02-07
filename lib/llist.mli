type 'a list_lens =
  | Filter of 'a list * 'a list * int list
  | Map of 'a list * 'a list * int list

exception EmptyCombinationException

val filter : ('a -> bool) -> 'a list -> 'a list_lens
val map : ('a -> 'a) -> 'a list -> 'a list_lens

val combine : ('a list -> 'a list_lens) list -> 'a list -> 'a list_lens
(** Given [a; b; c; d] combines d(c(b(a x))) *)

val under: ('a list -> 'a list_lens) -> 'a list -> 'a list
