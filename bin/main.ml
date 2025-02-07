open Lens.List_lens

let a = [ 1; 2; 3; 4; 5 ]
let f = filter (fun x -> x mod 2 == 1)
let m = map (fun x -> x + 1)
let c = combine [ f; m ]
let ris = apply c a;;

List.iter (fun x -> Printf.printf "%d " x) a;;
Printf.printf "\n";;
List.iter (fun x -> Printf.printf "%d " x) ris;;
Printf.printf "\n"
