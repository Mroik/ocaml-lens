type 'a list_lens =
  | Filter of 'a list * 'a list * int list
  | Map of 'a list * 'a list * int list

type lens_error = NotSameShape

exception NotSameShapeException
exception EmptyCombinationException

let flatten_lens a b =
  match (a, b) with
  | Filter (orig_a, _, i_a), Filter (_, fil_b, i_b) ->
      let actual_i =
        List.filteri (fun i _ -> List.exists (fun j -> i == j) i_b) i_a
      in
      Ok (Filter (orig_a, fil_b, actual_i))
  | Filter (orig_a, _, i_a), Map (_, new_b, _) -> Ok (Map (orig_a, new_b, i_a))
  | Map (orig_a, _, i_a), Filter (_, fil_b, i_b) ->
      let actual_i =
        List.filteri (fun i _ -> List.exists (fun j -> j == i) i_b) i_a
      in
      Ok (Filter (orig_a, fil_b, actual_i))
  | Map (orig_a, _, i_a), Map (_, new_b, _) -> Ok (Map (orig_a, new_b, i_a))

let filter f l =
  let cop =
    List.mapi (fun i e -> (i, e)) l |> List.filter (fun (_, e) -> f e)
  in
  let index = List.map (fun (i, _) -> i) cop in
  let ris = List.map (fun (_, e) -> e) cop in
  Filter (l, ris, index)

let map f l =
  let cop = List.mapi (fun i e -> (i, f e)) l in
  let index = List.map (fun (i, _) -> i) cop in
  let ris = List.map (fun (_, e) -> e) cop in
  Map (l, ris, index)

(** Given [a; b; c; d] combines d(c(b(a x))) *)
let combine funcs =
  let rec loop prev other =
    match other with
    | n :: l ->
        let pp =
         fun s ->
          let p_ris = prev s in
          let va =
            match p_ris with
            | Map (_, b, _) -> n b
            | Filter (_, b, _) -> n b
          in
          match flatten_lens p_ris va with
          | Ok a -> a
          | Error _ -> raise NotSameShapeException
        in
        loop pp l
    | _ -> prev
  in

  match funcs with
  | n :: [] -> n
  | n :: l -> loop n l
  | [] -> raise EmptyCombinationException

let apply f l =
  match f l with
  | Filter (_, new_items, indexes)->
      let vvv = List.combine indexes new_items in
      List.mapi
        (fun i e -> match List.assoc_opt i vvv with Some a -> a | None -> e)
        l
  | Map (_, new_items, indexes) ->
      let vvv = List.combine indexes new_items in
      List.mapi
        (fun i e -> match List.assoc_opt i vvv with Some a -> a | None -> e)
        l

(** Infix for `combine` *)
let ( >>| ) = combine
