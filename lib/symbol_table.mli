(* Thrown by the add_entry function when there is already a symbol with that identifier in the current block. *)
exception DuplicateEntry of Ast.identifier 
(* Thrown when the lookup fails on a given identifier. *)
exception MissingEntry of Ast.identifier

type 'a t 

val empty_table : 'a t 
val begin_block : 'a t -> 'a t 
val end_block : 'a t -> 'a t
val add_entry : Ast.identifier -> 'a -> 'a t -> 'a t
val lookup : Ast.identifier -> 'a t -> 'a
val of_alist : (Ast.identifier * 'a) list -> 'a t 

(* Debugging functions, not meant to be used in production.  *)
val get_bindings_block: 'a t -> (Ast.identifier * 'a) list
val show : 'a t -> string