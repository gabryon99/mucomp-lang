exception DuplicateEntry of Ast.identifier
exception MissingEntry of Ast.identifier

module StrMap = Map.Make(String)

(* t = sym_table *)
type 'a t = 
  | EmptySymbolTable 
  | SymbolTable of { parent: 'a t; table: 'a StrMap.t }

let empty_table = EmptySymbolTable

let begin_block = function
| EmptySymbolTable as p -> SymbolTable({parent = p; table = StrMap.empty})
| SymbolTable(_) as p -> SymbolTable({parent = p; table = StrMap.empty})

let end_block = function
| EmptySymbolTable -> failwith "Cannot end a block because the table is empty"
| SymbolTable({parent = p; _}) -> p

let add_entry (id: Ast.identifier) v = function
| EmptySymbolTable -> failwith "Cannot add an entry into a empty symbol table"
| SymbolTable({parent = p; table = t}) -> 
    if StrMap.mem id t then
        raise (DuplicateEntry(id))
    else 
        let t = StrMap.add id v t in
        SymbolTable({parent = p; table = t})

let update_entry (id: Ast.identifier) v = function
| EmptySymbolTable -> failwith "Cannot update an entry into a empty symbol table"
| SymbolTable({parent = p; table = t}) -> 
    if not (StrMap.mem id t) then
        raise (MissingEntry(id))
    else 
        let t = StrMap.add id v t in
        SymbolTable({parent = p; table = t})

let rec lookup (id: Ast.identifier) = function 
| EmptySymbolTable -> raise (MissingEntry(id))
| SymbolTable({parent = p; table = t}) ->
    if StrMap.mem id t then
      StrMap.find id t
    else
        lookup id p
        
let of_alist l =
    let new_st = begin_block empty_table in
    let acc st (id, v) = add_entry id v st in
    List.fold_left acc new_st l

let get_bindings_block = function
| EmptySymbolTable -> []
| SymbolTable({table = table; _}) ->
  List.map (fun x -> x) (StrMap.bindings table)

let show tbl =
  let rec aux acc = function
  | EmptySymbolTable -> (acc ^ "[]")
  | SymbolTable({parent; table}) ->
      let aux_str acc (id, _) = id ^ "," ^ acc in
      let identifiers = Seq.fold_left (aux_str) "" (StrMap.to_seq table) in
      let msg = Printf.sprintf "[%s] -> " identifiers in
      aux (acc ^ msg) parent
  in aux "" tbl