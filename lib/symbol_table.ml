exception DuplicateEntry of Ast.identifier
exception MissingEntry of Ast.identifier

(* t = sym_table *)
type 'a t = 
  | EmptySymbolTable 
  | SymbolTable of { parent: 'a t; table: (Ast.identifier, 'a) Hashtbl.t }

let empty_table = EmptySymbolTable

let begin_block = function
| EmptySymbolTable as p -> SymbolTable({parent = p; table = Hashtbl.create 0})
| SymbolTable(_) as p -> SymbolTable({parent = p; table = Hashtbl.create 0})

let end_block = function
| EmptySymbolTable -> failwith "Cannot end a block because the table is empty"
| SymbolTable({parent = p; _}) -> p

let add_entry (id: Ast.identifier) v = function
| EmptySymbolTable -> failwith "Cannot add an entry into a empty symbol table"
| SymbolTable({parent = p; table = t}) -> 
    if Hashtbl.mem t id then
        raise (DuplicateEntry(id))
    else 
        Hashtbl.add t id v;
        SymbolTable({parent = p; table = t})

let rec lookup (id: Ast.identifier) = function 
| EmptySymbolTable -> raise (MissingEntry(id))
| SymbolTable({parent = p; table = t}) ->
    if Hashtbl.mem t id then
      Hashtbl.find t id
    else
        lookup id p
        
let of_alist l =
    let new_st = begin_block empty_table in
    let acc st (id, v) = add_entry id v st in
    List.fold_left acc new_st l

(* Debugging functions, not meant to be used in production. *)

let show tbl =
  let rec aux acc = function
  | EmptySymbolTable -> (acc ^ "[]")
  | SymbolTable({parent; table}) ->
      let aux_str acc (id, _) = id ^ "," ^ acc in
      let identifiers = Seq.fold_left (aux_str) "" (Hashtbl.to_seq table) in
      let msg = Printf.sprintf "[%s] -> " identifiers in
      aux (acc ^ msg) parent
  in aux "" tbl