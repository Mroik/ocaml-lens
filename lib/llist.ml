type 'a list_lens_internals =
  | Filter of 'a list * int list
  | Map of 'a list * int list

exception EmptyCombinationException
exception NotSameShapeException

let flatten_lens a b =
  match (a, b) with
  | Filter (_, i_a), Filter (fil_b, i_b) ->
      let actual_i =
        List.filteri (fun i _ -> List.exists (fun j -> i == j) i_b) i_a
      in
      Filter (fil_b, actual_i)
  | Filter (_, i_a), Map (new_b, _) -> Map (new_b, i_a)
  | Map (_, i_a), Filter (fil_b, i_b) ->
      let actual_i =
        List.filteri (fun i _ -> List.exists (fun j -> j == i) i_b) i_a
      in
      Filter (fil_b, actual_i)
  | Map (_, i_a), Map (new_b, _) -> Map (new_b, i_a)

let filter f l =
  let cop =
    List.mapi (fun i e -> (i, e)) l |> List.filter (fun (_, e) -> f e)
  in
  let index = List.map (fun (i, _) -> i) cop in
  let ris = List.map (fun (_, e) -> e) cop in
  Filter (ris, index)

let map f l =
  let index = List.init (List.length l) (fun i -> i) in
  let ris = List.map f l in
  Map (ris, index)

let combine funcs =
  let rec loop prev other =
    match other with
    | n :: l ->
        let pp =
         fun s ->
          let p_ris = prev s in
          let va =
            match p_ris with Map (b, _) -> n b | Filter (b, _) -> n b
          in
          flatten_lens p_ris va
        in
        loop pp l
    | _ -> prev
  in

  match funcs with
  | n :: [] -> n
  | n :: l -> loop n l
  | [] -> raise EmptyCombinationException

let under transformer filter =
 fun x ->
  let proc, indexes =
    match filter x with
    | Filter (proc, indexes) -> (proc, indexes)
    | Map (proc, indexes) -> (proc, indexes)
  in
  let ris = transformer proc in
  if List.length ris != List.length proc then raise NotSameShapeException
  else
    let cop = List.combine indexes ris in
    List.mapi
      (fun i e -> match List.assoc_opt i cop with Some a -> a | None -> e)
      x

let ( >>| ) = under
