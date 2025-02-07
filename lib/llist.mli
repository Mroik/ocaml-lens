type 'a list_lens_internals

exception EmptyCombinationException
exception NotSameShapeException

val filter : ('a -> bool) -> 'a list -> 'a list_lens_internals
val map : ('a -> 'a) -> 'a list -> 'a list_lens_internals

val combine :
  ('a list -> 'a list_lens_internals) list -> 'a list -> 'a list_lens_internals
(** Given [a; b; c; d] combines d(c(b(a x))) *)

val under :
  ('a list -> 'a list) -> ('b -> 'a list_lens_internals) -> 'b -> 'a list

val ( >>| ) :
  ('a list -> 'a list) -> ('b -> 'a list_lens_internals) -> 'b -> 'a list
(** Returns a new function that uses the one on the right to pre-process the
    list, applies the one on the left on the result and then undoes the
    pre-processing. *)
