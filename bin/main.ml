open Lens.Llist

let a = [ 1; 2; 3; 4; 5 ]
let f = filter (fun x -> x mod 2 == 1)
let m = map (fun x -> x + 1)
let f2 = filter (fun x -> x < 3)
let c = combine [ f; m; f2 ]
let ris = (List.map (fun x -> x * 2) >>| c) a;;

List.map (fun x -> string_of_int x) a
|> String.concat " " |> Printf.printf "%s\n"
;;

List.map (fun x -> string_of_int x) ris
|> String.concat " " |> Printf.printf "%s\n"
