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

let name_mangle id typ = 
  Printf.sprintf "%s$%s" id (show_typ typ)