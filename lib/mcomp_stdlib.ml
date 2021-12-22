open Ast 

let prelude_signature = [
  "print", TFun([TInt],TVoid);
  "getint", TFun([], TInt)
]

let app_signature = [
  "main", TFun([],TInt)
]

let list_remove element l = 
  let rec aux acc = function
  | [] -> acc
  | h::t -> if h = element then aux acc t else aux (h::acc) t
in aux [] l

let list_are_disjoint l1 l2 =
  let rec aux = function
  | [] -> true
  | h::t -> not(List.mem h l2) && aux t in 
  aux l1

let name_mangle id typ = 
  Printf.sprintf "%s$%s" id (show_typ typ)