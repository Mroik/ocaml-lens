open Lens.Llist

let a = [ 1; 2; 3; 4; 5 ]
let f = filter (fun x -> x mod 2 == 1)
let m = map (fun x -> x + 1)
let f2 = filter (fun x -> x < 3)
let c = combine [ f; m; f2; m ]
let ris = under c a;;

List.iter (fun x -> Printf.printf "%d " x) a;;
Printf.printf "\n";;
List.iter (fun x -> Printf.printf "%d " x) ris;;
Printf.printf "\n"
