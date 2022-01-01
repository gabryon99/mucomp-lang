open Ast 

let prelude_signature = [
  "print", TFun([TInt],TVoid);
  "putc", TFun([TChar], TVoid);
  "putendl", TFun([], TVoid);
  "print_err", TFun([TInt],TVoid);
  "getint", TFun([], TInt);
  "getchar", TFun([], TChar);
  "abort", TFun([TInt], TVoid);
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

let list_zip_with_index l1 =
  let rec aux idx acc = function
  | [] -> acc
  | h::t -> aux (idx + 1) ((idx, h)::acc) t in 
  aux 0 [] l1