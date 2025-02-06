type ('a, 'b) list_lens =
  | Filter of 'a list * 'a list * int list
  | Find of 'a list * 'a option * int option
  | Map of 'a list * 'b list * int list

type lens_error = NotSameShape

let flatten_lens a b =
  match (a, b) with
  | Filter (orig_a, fil_a, i_a), Filter (orig_b, fil_b, i_b) ->
      if List.length fil_a != List.length orig_b then Error NotSameShape
      else
        let actual_i =
          List.filteri (fun i _ -> List.exists (fun j -> i == j) i_b) i_a
        in
        Ok (Filter (orig_a, fil_b, actual_i))
  | Filter (orig_a, _, _), Find (_, found_b, None) ->
      Ok (Find (orig_a, found_b, None))
  | Filter (orig_a, _, i_a), Find (_, found_b, Some i_b) ->
      let actual_i = List.filteri (fun i _ -> i == i_b) i_a |> List.hd in
      Ok (Find (orig_a, found_b, Some actual_i))
  | Filter (orig_a, _, i_a), Map (_, new_b, _) -> Ok (Map (orig_a, new_b, i_a))
  (* TODO Rest of the matches *)
  | _ -> Error NotSameShape
