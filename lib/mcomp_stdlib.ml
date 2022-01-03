open Ast 

let prelude_signature = [
  "time",           TFun([], TInt);
  "rand",           TFun([], TInt);
  "putendl",        TFun([], TVoid);
  "getint",         TFun([], TInt);
  "getchar",        TFun([], TChar);
  "set_rand_seed",  TFun([TInt], TVoid);
  "print",          TFun([TInt], TVoid);
  "print_err",      TFun([TInt], TVoid);
  "abort",          TFun([TInt], TVoid);
  "putc",           TFun([TChar], TVoid);
]

let app_signature = [
  "main", TFun([],TInt)
]

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

(* 
* If an LLVM module contains error, the stdout will be deleted, therefore 
* the logged messages are gone. To address this problem we use
* a file called 'debug.log' to log the message and prevent their lost.
*)
let debug_fd = open_out "debug.log"

(* Auxiliar function to print logs inside debug file *)
let print_debug = Printf.fprintf debug_fd 

(* At the program exit prints a log and close the debug file descriptor *)
let _ = at_exit (fun _ ->
  print_debug "Terminating debug\n";
  close_out debug_fd
) 