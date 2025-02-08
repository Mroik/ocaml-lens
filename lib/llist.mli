exception EmptyCombinationException
exception NotSameShapeException

type 'a list_lens_internals
type ('a, 'b) list_lens

val filter : ('a -> bool) -> ('a, 'a) list_lens
val map : ('a -> 'a) -> ('a, 'a) list_lens

val combine : ('a, 'a) list_lens list -> ('a, 'a) list_lens
(** Given [a; b; c; d] combines d(c(b(a x))).
    @raise EmptyCombinationException *)

val under : ('a list -> 'b list) -> ('b, 'a) list_lens -> 'b list -> 'b list
(** Given a ('a list -> 'b list) and a ('b, 'a) list_lens returns a new function
    that uses the list_lens to pre-process the list, applies the mapping
    function on the result and then undoes the pre-processing. The function
    doing the modifying has to keep the same shape of the pre-processed list,
    raises NotSameShapeException if otherwise.
    @raise NotSameShapeException *)

val ( >>| ) : ('a list -> 'b list) -> ('b, 'a) list_lens -> 'b list -> 'b list
(** Infix operator for the under function. *)
