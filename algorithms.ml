(* First duplicate *)
let rec exists element list =
  match list with
  | [] -> false
  | hd::tl -> element = hd || exists element tl

let rec first_duplicate list =
  match list with
  | [] -> -1000
  | hd::tl -> if (exists hd tl) then hd else first_duplicate tl
  
(* First non-repeating *) 
let rec first_nonrepeating_helper list duplicate_list = 
  if list = [] then
    -10000
  else 
  if List.mem(List.hd list)(List.tl list) || List.mem(List.hd list)(duplicate_list) then
    first_nonrepeating_helper(List.tl list)(List.hd list::duplicate_list) 
  else
    (List.hd list);;
let rec first_nonrepeating = function(list) ->
  first_nonrepeating_helper list [];;

(* Sum of two *)
let rec arr x a = match a with
    [] -> false
  | (y::ys) -> x=y || arr x ys;; 
let rec sumOfTwo(b,a,v) = match b with
    [] -> false
  | (x::xs) -> arr (v-x) a || sumOfTwo(xs,a,v);;
  
(* CYK sublist *)
let rec cyk_sublist_helper n a = match n with
    0 -> []
  | x -> (x, (a-x)) :: (cyk_sublist_helper (x-1) a);;

let cyk_sublists n = cyk_sublist_helper (n-1) n;;