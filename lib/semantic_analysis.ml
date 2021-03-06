exception Semantic_error of Location.code_pos * string

type attr = {id: Ast.identifier; loc: Location.code_pos; typ: Ast.typ}
type 'a sym = 
  | SComponent of {cattr: 'a; csym_tbl: 'a sym_table; cprov: 'a sym_table; cuses: 'a sym_table;}
  | SInterface of {iattr: 'a; isym_tbl: 'a sym_table}
  | SFunction  of {fattr: 'a; fsym_tbl: 'a sym_table}
  | SVar       of {vattr: 'a}
and 'a sym_table = ('a sym) Symbol_table.t

let (@>) node annot = Ast.make_node node annot

(** Function used inside pattern matching for cases that are not eligible *)
let ignore_pattern () = failwith "Should not happen"

type 'a fun_env = {
  component_ast_node: 'a Ast.component_decl_node;
  component_sym: attr sym;
  current_symbol_table: attr sym_table;
}

(** The function performs semantic-rule checks as described by the specification. *)
let first_pass ast global_table = 
  (* Create a new interface symbol and new interface decleration from an interface signature *)
  let load_standard_interface prelude_name prelude_list global_table =
    (* Create function symbols *)
    let function_symbols = List.map (
      fun (fname, ftyp) ->
        let fattr = {id = fname; loc = Location.dummy_code_pos; typ = ftyp} in 
        let fsym = SFunction({fattr = fattr; fsym_tbl = Symbol_table.empty_table}) in
        (fname, fsym)
    ) prelude_list in
    let member_node_list = List.map (
      fun (fname, ftyp) ->
        Ast.FunDecl({Ast.rtype = ftyp; fname = fname; formals = []; body = None}) @> Location.dummy_code_pos
    ) prelude_list in
    (* Define interface symbol and node declaration *)
    let iattr = {id = prelude_name; loc = Location.dummy_code_pos; typ = Ast.TInterface(prelude_name)} in 
    let isym_tbl = Symbol_table.of_alist function_symbols in 
    let isym = SInterface({iattr = iattr; isym_tbl}) in
    let inode = (Ast.InterfaceDecl({iname = prelude_name; declarations = member_node_list})) @> Location.dummy_code_pos in
    (Symbol_table.add_entry prelude_name isym global_table, inode)
  in
  let rec visit_interface_declarations isym_tbl iname = function
  | [] -> isym_tbl
  | annotated_node::tail ->
      let node = annotated_node.Ast.node in 
      let loc = annotated_node.Ast.annot in 
      (* Create symbols for function and field variables *)
      let (sid, symbol) = match node with 
      | Ast.FunDecl({Ast.rtype; fname; formals; _}) ->
        let formals_types = List.map (fun (_, t) -> t) formals in 
        let fattr = {id = fname; loc = loc; typ = Ast.TFun(formals_types, rtype)} in
        let fsym_tbl = Symbol_table.empty_table in
        let fsym = SFunction({fattr = fattr; fsym_tbl = fsym_tbl}) in
        (fname, fsym)
      | Ast.VarDecl((vid, vtyp), None) ->
        let vattr = {id = vid; loc = loc; typ = vtyp} in
        let vsym = SVar({vattr = vattr}) in
        (vid, vsym)
      | Ast.VarDecl(_, _) -> ignore_pattern ()
      in
      try
        (* Try to insert the member symbol inside the interface symbol table and move to next member *)
        visit_interface_declarations (Symbol_table.add_entry sid symbol isym_tbl) iname tail
      with Symbol_table.DuplicateEntry(_) ->
        let msg = Printf.sprintf "Double identifier `%s` found inside `%s` interface members" sid iname in
        raise (Semantic_error(loc, msg))
  in
  let rec visit_interfaces global_table = function
  | [] -> global_table
  | annotated_node::tail ->
      let node = annotated_node.Ast.node in
      let loc = annotated_node.Ast.annot in
      match node with
      | Ast.InterfaceDecl({iname; declarations}) ->
        (* Create attributes for the interface *)
        let iattr = {id = iname; loc = loc; typ = Ast.TInterface(iname)} in
        (* Create a new symbol table for the interface *)
        let isym_tbl = Symbol_table.begin_block (Symbol_table.empty_table) in
        let isym_tbl = visit_interface_declarations isym_tbl iname declarations in
        (* Create a new symbol to be inserted inside the global symbol table *)
        let isym = SInterface({iattr = iattr; isym_tbl = isym_tbl}) in
        (* Visit the remaining interfaces *)
        visit_interfaces (Symbol_table.add_entry iname isym global_table) tail
  in
  let rec visit_components global_table = function
  | [] -> global_table
  | annotated_node::tail ->
    let node = annotated_node.Ast.node in 
    let loc = annotated_node.Ast.annot in 
    match node with
    | Ast.ComponentDecl({cname; uses; provides; definitions}) ->
      let rec visit_prov_uses sym_tbl tbl_name = function
      | [] -> sym_tbl
      | iname::xs ->
        begin
          try
            let iface_sym = Symbol_table.lookup iname global_table in
            match iface_sym with
            | SInterface(_) -> visit_prov_uses (Symbol_table.add_entry iname iface_sym sym_tbl) tbl_name xs
            | SComponent({cattr = {id; _;}; _}) -> 
              let msg = Printf.sprintf "Found a component `%s` in component %s %s list. Only interface can be there!" id cname tbl_name in 
              raise (Semantic_error(loc, msg))
            | _ -> ignore_pattern ()
          with 
          | Symbol_table.MissingEntry(_) ->
            (* There cannot be undefined interface... *)
            let msg = Printf.sprintf "Undefined interface `%s` in %s component `%s` list! Did you declare the interface?" iname tbl_name cname in
            raise (Semantic_error(loc, msg))
          | Symbol_table.DuplicateEntry(_) ->
            (* The name of an interface must occur only once in a `provides` and `uses` list *)
            let msg = Printf.sprintf "Duplicated interface `%s` in %s component `%s` list." iname tbl_name cname in
            raise (Semantic_error(loc, msg))
        end
      in
      (** Visit the member definitions recursively and put them inside the component symbol table *)
      let rec visit_member_definitions csym_tbl = function
      | [] -> csym_tbl
      | annotated_node::tail -> 
        let node = annotated_node.Ast.node in 
        let loc = annotated_node.Ast.annot in 
        match node with
        | Ast.FunDecl({Ast.rtype; fname; formals; _}) ->
          let aux_fold fsym_tbl (vid, vtyp) =
            match (vtyp) with
            | Ast.TVoid -> raise (Semantic_error(loc, "The formal parameters cannot be a void type!"))
            | Ast.TArray(_, Some _) -> raise (Semantic_error(loc, "Array references do not need a size!"))
            | _ -> 
              let vattr = {id = vid; loc = loc; typ = vtyp} in
              let vsym = SVar({vattr = vattr}) in
              try
                Symbol_table.add_entry vid vsym fsym_tbl
              with Symbol_table.DuplicateEntry(_) ->
                let msg = Printf.sprintf "Double formal parameter definition. The paramater `%s` is already defined inside the formals list." vid in 
                raise (Semantic_error(loc, msg))
          in
          let formals_types = List.map (fun (_, t) -> t) formals in 
          let ftyp = Ast.TFun(formals_types, rtype) in
          let fattr = {id = fname; loc = loc; typ = ftyp} in
          let fsym_tbl = Symbol_table.begin_block (Symbol_table.empty_table) in
          (* fill function symbol table with arguments... *)
          let fsym_tbl = List.fold_left aux_fold fsym_tbl formals in
          (* create new function symbol *)
          let fsym = SFunction({fattr = fattr; fsym_tbl = fsym_tbl}) in
          begin
            try
              visit_member_definitions (Symbol_table.add_entry fname fsym csym_tbl) tail
            with Symbol_table.DuplicateEntry(_) ->
              let msg = Printf.sprintf "Double function definition. The function `%s` was already defined in the current scope!" fname in
              raise (Semantic_error(loc, msg))
          end
        | Ast.VarDecl((vid, vtyp), _) ->
          begin
            match vtyp with
            | Ast.TVoid -> raise (Semantic_error(loc, "Variables of type void cannot be declared!"))
            | Ast.TArray(_, Some n) when n <= 0 -> raise (Semantic_error(loc, "Array size must be larger than 1!"))
            | Ast.TArray(t, _) when not(Ast.is_scalar_type t) -> raise (Semantic_error(loc, "Only arrays of scalar types are allowed!"))
            | _ ->
              let vattr = {id = vid; loc = loc; typ = vtyp} in
              let vsym = SVar({vattr = vattr}) in
              try
                visit_member_definitions (Symbol_table.add_entry vid vsym csym_tbl) tail 
              with Symbol_table.DuplicateEntry(_) ->
                raise (Semantic_error(loc, "Double field variable declerations!"))
          end
      in
      (* Check if the provides list has the prelude interface *)
      if List.mem Mcomp_stdlib.g_PRELUDE_ID provides then
        raise (Semantic_error(loc, "The Prelude interface cannot be provided by any component!"))
      else
        (* Let's ensure provides and uses lists are disjoint *)
        if not (Mcomp_stdlib.list_are_disjoint provides uses) then
          raise (Semantic_error(loc, "Uses and provides lists are not disjoint!"))
        else
          (* Create components attribute... *)
          let cattr = {id = cname; loc = loc; typ = Ast.TComponent(cname)} in
          (* Create a symbol table containing interface references provided by the component *)
          let cprov = Symbol_table.begin_block (Symbol_table.empty_table) in 
          let cprov = visit_prov_uses cprov "provides" provides in
          let cuses = Symbol_table.begin_block (Symbol_table.empty_table) in
          let cuses = visit_prov_uses cuses "uses" (Mcomp_stdlib.g_PRELUDE_ID :: uses) in
          (* Build component symbol table containing all the definitions inside a component (fields/functions) *)
          let csym_tbl = Symbol_table.begin_block (Symbol_table.empty_table) in 
          let csym_tbl = visit_member_definitions csym_tbl definitions in
          (* Create a new symbol for the component and push it inside the symbol table *)
          let csym = SComponent({cattr = cattr; csym_tbl = csym_tbl; cprov = cprov; cuses = cuses}) in
          try
            visit_components (Symbol_table.add_entry cname csym global_table) tail
          with Symbol_table.DuplicateEntry(_) ->
            (* Check for duplicated symbol identifier *)
            begin
              (* A MissingEntry exception cannot be thrown since the name is contained inside the table already *)
              let dup_sym = Symbol_table.lookup cname global_table in 
              match dup_sym with 
              | SInterface(_) -> 
                let msg = Printf.sprintf "Component name not valid since it seems that an interface named` %s` already exists." cname in
                raise (Semantic_error(loc, msg))
              | SComponent(_) ->
                let msg = Printf.sprintf "Duplicated component name. It seems that a component named` %s` already exists." cname in
                raise (Semantic_error(loc, msg))
              | _ -> ignore_pattern ()
            end
  in
  let rec find_interface name = function
  | [] -> None
  | ({Ast.node = (Ast.InterfaceDecl({iname; _}) as iface); _})::tail -> if iname = name then Some iface else find_interface name tail 
  in
  let rec check_component_provides global_table interfaces = function
  | [] -> global_table
  | annotated_node::tail -> 
    let node = annotated_node.Ast.node in 
    let loc = annotated_node.Ast.annot in
    match node with
    | Ast.ComponentDecl({cname; provides; _}) ->
      (* Take component symbol from the global symbol table *)
      let cysm_tbl = (match Symbol_table.lookup cname global_table with SComponent({csym_tbl; _}) -> csym_tbl | _ -> ignore_pattern ()) in 
      (* Extract the interface nodes from the AST *)
      let interfaces_nodes = List.map (fun iname -> find_interface iname interfaces) provides in
      let _ = List.iter(fun i ->
        match i with 
        | None -> ()
        | Some iface ->
        let (iname, ideclarations) = (match (iface) with Ast.InterfaceDecl({iname; declarations}) -> (iname, declarations)) in
        let isym_tbl = (match Symbol_table.lookup iname global_table with SInterface({isym_tbl; _}) -> isym_tbl | _ -> ignore_pattern ()) in
        (* Check if the component provides each field of a given interface *)
        List.iter (fun imember -> 
          let imember_node = imember.Ast.node in 
          match imember_node with
          | Ast.FunDecl({Ast.fname; _}) ->
            let ifsym = Symbol_table.lookup fname isym_tbl in
            begin
              try
                (* Is the function defined inside the component? *)
                let cfsym = Symbol_table.lookup fname cysm_tbl in
                match (ifsym, cfsym) with
                | (SFunction({fattr = {typ = iftyp; _}; _}), SFunction({fattr = {typ = cityp; loc = cloc; _}; _})) -> 
                  (* Is the function type equal to the one described by the interface? *)
                  if not(Ast.equal_typ iftyp cityp) then
                    let msg = Printf.sprintf "The function `%s.%s` type mismatch with the one defined in the interface `%s`!" cname fname iname in 
                    raise (Semantic_error(cloc, msg))
                | (_, _) -> ignore_pattern ()
              with Symbol_table.MissingEntry(_) -> 
                let msg = Printf.sprintf "The function `%s` defined inside the interface `%s` was not implemented inside the component `%s`!" fname iname cname in
                raise (Semantic_error(loc, msg))
            end
          | Ast.VarDecl((ivid, _), _) ->
            let ivsym = Symbol_table.lookup ivid isym_tbl in
            begin
              try
                (* Is the variable defined inside the component? *)
                let cvsym = Symbol_table.lookup ivid cysm_tbl in
                match (ivsym, cvsym) with
                | (SVar({vattr = {typ = ivtyp; _}}), SVar({vattr = {typ = cvtyp; loc = cloc; _}})) -> 
                  (* Is the variable type equal to the one described by the interface? *)
                  if not(Ast.equal_typ ivtyp cvtyp) then
                    let msg = Printf.sprintf "The field `%s.%s` type mismatch with the one defined in the interface `%s`!" cname ivid iname in 
                    raise (Semantic_error(cloc, msg))
                | (_, _) -> ignore_pattern ()
              with Symbol_table.MissingEntry(_) -> 
                let msg = Printf.sprintf "The field `%s` defined inside the interface `%s` was not implemented inside the component `%s`!" ivid iname cname in
                raise (Semantic_error(loc, msg))
            end
        ) ideclarations
      ) interfaces_nodes in
      check_component_provides global_table interfaces tail
  in
  let rec check_component_uses global_table interfaces = function 
  | [] -> global_table
  | annotated_node::tail ->
    let node = annotated_node.Ast.node in 
    let loc = annotated_node.Ast.annot in 
    match node with
    | Ast.ComponentDecl({cname; uses; _}) ->
      let check_name_clahses used_interfaces = 
        let rec aux table = function
        | [] -> ()
        | iface::tail ->
          (* Insert interface member definitions into the symbol table *)
          let (iname, idecls) = match iface with Ast.InterfaceDecl({iname; declarations;}) -> (iname, declarations) in
          let table = List.fold_left (fun acc member -> 
            match (member.Ast.node) with
            | Ast.FunDecl({Ast.fname = id; _})
            | Ast.VarDecl((id, _), _) -> 
              try
                (* Try to add the name to the symbol table *)
                Symbol_table.add_entry id iname acc
              with Symbol_table.DuplicateEntry(_) ->
                (* If the add operation failed it means that there is a name clasing between interface members *)
                let old_interface_name = Symbol_table.lookup id acc in
                let msg = Printf.sprintf "The name '%s' deriving from the used interface '%s' collides with the one coming from '%s'!" id iname old_interface_name in
                raise (Semantic_error(loc, msg))
          ) table idecls in
          aux table tail
        in
        aux (Symbol_table.empty_table |> Symbol_table.begin_block) used_interfaces
      in
      (* Check if the App interface is used! *)
      if List.mem Mcomp_stdlib.g_APP_ID uses then
        let msg = Printf.sprintf "The %s interface cannot be used, but just provided once!" Mcomp_stdlib.g_APP_ID in
        raise (Semantic_error(loc, msg))
      else
      (* Get the component symbol table *)
      let cysm_tbl = (match Symbol_table.lookup cname global_table with SComponent({csym_tbl; _}) -> csym_tbl | _ -> ignore_pattern ()) in 
      (* Get a list of interfaces nodes from uses list *)
      let interfaces_nodes = List.fold_left (fun acc iname -> 
        match find_interface iname interfaces with 
        | None -> acc
        | Some(i) -> i::acc
      ) [] (Mcomp_stdlib.g_PRELUDE_ID :: uses) |> List.rev in
      (* Check name clashes... *)
      let _ = check_name_clahses interfaces_nodes in
      (* Check if the component define function member coming by used interfaces *)
      let _ = List.iter(fun iface ->
        (* Get the interface name and decleration from th interface node *)
        let (iname, ideclarations) = (match (iface) with Ast.InterfaceDecl({iname; declarations}) -> (iname, declarations)) in
          Mcomp_stdlib.print_debug (iname);
          (* For each decleration node, check if the function is defined inside the component symbol table *)
          List.iter(fun imember ->
            let aux' symbol_type name cysm_tbl =
              try
                let _ = Symbol_table.lookup name cysm_tbl in 
                let msg = Printf.sprintf "Invalid %s identifier `%s` since the component uses `%s` interface which declares %s member!" symbol_type name iname symbol_type in
                raise (Semantic_error(loc, msg))
              with Symbol_table.MissingEntry(_) -> 
                () 
            in
            match imember.Ast.node with
            | Ast.FunDecl({Ast.fname; _}) -> aux' "function" fname cysm_tbl
            | Ast.VarDecl((vname, _), _) -> aux' "variable" vname cysm_tbl
          ) ideclarations
      ) interfaces_nodes in
      check_component_uses global_table interfaces tail
  in
  let rec check_app_interface_existance global_table app_provided = function
  | [] ->
    if not app_provided then
      raise (Semantic_error(Location.dummy_code_pos, "No component provided the App interface."))
    else
      global_table
  | annotated_node::tail ->
    let node = annotated_node.Ast.node in 
    let loc = annotated_node.Ast.annot in 
    match node with
    | Ast.ComponentDecl({provides; _}) ->
      (* Does the component provide the App interface? *)
      let temp = List.mem Mcomp_stdlib.g_APP_ID provides in
      if app_provided && temp then
        let msg = "There was already a component providing the `App` interface." in
        raise (Semantic_error(loc, msg))
      else
        check_app_interface_existance global_table (app_provided || temp) tail
  in
  let (interfaces, components) = (match ast with Ast.CompilationUnit({interfaces; components; _}) -> (interfaces, components)) in
  (* Based the global table table containing the interface symbols, let's get a string map that will be used later with components *)
  (* Preload the `Prelude` and `App` interface into the global symbol table *)
  let (global_table, prelude_inode) = load_standard_interface Mcomp_stdlib.g_PRELUDE_ID Mcomp_stdlib.prelude_signature global_table in 
  let (global_table, _) = load_standard_interface Mcomp_stdlib.g_APP_ID Mcomp_stdlib.app_signature global_table in 
  (* Load the interface and component definitions *)
  let global_table = visit_interfaces global_table interfaces in
  let global_table = visit_components global_table components in
  (* Perform some checks *)
  let global_table = check_component_provides global_table interfaces components in
  let global_table = check_component_uses global_table (prelude_inode :: interfaces) components in
  (* Is the App interface provided by just one component? *)
  let global_table = check_app_interface_existance global_table false components in
  global_table

let check_local_decl_type annotated_node =
  let node = annotated_node.Ast.node in 
  let loc = annotated_node.Ast.annot in 
  let local_decl_typ = match node with Ast.LocalDecl((_, t), _) -> t | _ -> ignore_pattern () in
  match local_decl_typ with 
  | Ast.TVoid -> raise (Semantic_error(loc, "Cannot declare variables of type void!"))
  | Ast.TArray(_, Some n) when n <= 0 -> raise (Semantic_error(loc, "The array size cannot be less than 0!"))
  | Ast.TArray(t, _) when not(Ast.is_scalar_type t) -> raise (Semantic_error(loc, "Only arrays of scalar types are allowed!"))
  | _ -> ()

let check_fun_return_type annotated_node = 
  let node = annotated_node.Ast.node in
  let loc = annotated_node.Ast.annot in
  let rtype = match node with Ast.FunDecl({Ast.rtype = rtype; _}) -> rtype | _ -> ignore_pattern () in
  match rtype with 
  | Ast.TInt | Ast.TBool | Ast.TChar | Ast.TVoid | Ast.TFloat -> ()
  | _  -> raise (Semantic_error(loc, "A function can only returns int, float, bool, char or void."))

let type_check_assign typ1 typ2 =
  match (typ1, typ2) with 
  (* Case: T1 <- T2 <== T1=T2 && Scalar(T1) && Scalar(T2) *)
  | (t1, t2) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) && (Ast.is_scalar_type t2) -> Result.ok true
  (* Case: T1 <- &T2 <== Scalar(T1) && T1=T2 && !Ref(T2) *)
  | (t1, Ast.TRef(t2)) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) && not(Ast.is_ref t2) -> Result.ok true
  (* Case: &T1 <- &T2 <== T1==T2 && Scalar(T1) *)
  | (Ast.TRef(t1), Ast.TRef(t2)) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) -> Result.ok true
  (* Case: &T1 <- T2 <== T1==T2 && Scalar(T1) *)
  | (Ast.TRef(t1), t2) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) -> Result.ok true
  (* Error Cases *)
  | (Ast.TArray(_), Ast.TArray(_)) -> Result.error "Arrays cannot be assigned!"
  | (_, Ast.TVoid) -> Result.error "Cannot assign the void type to a variable!"
  | (t1, t2) -> Result.error (Printf.sprintf "Assignment not allowed. You cannot assign a %s to a %s variable!" (Ast.show_type t2) (Ast.show_type t1))

let rec type_check_lvalue fun_env annotated_lv = 

  let component_sym = fun_env.component_sym in 
  let component_ast_node = fun_env.component_ast_node in
  let function_sym_tbl = fun_env.current_symbol_table in

  let node = annotated_lv.Ast.node in 
  let loc = annotated_lv.Ast.annot in 

  match node with
  | Ast.AccIndex(lv, exp) ->
    let lv_new_node = type_check_lvalue fun_env lv in 
    begin
      match (lv_new_node) with
      | {Ast.node = Ast.AccIndex(_, _); _} -> ignore_pattern ()
      | {Ast.node = Ast.AccVar(_, _); annot = lv_typ} -> 
        let exp_new_node = type_check_expr fun_env exp in 
        (* Let's ensure exp_new_node has a TInt type *)
        begin
          match (exp_new_node) with 
          | {Ast.annot = Ast.TInt; _} -> 
            let final_typ = (match lv_typ with Ast.TArray(t, _) -> t | Ast.TRef(t) when Ast.is_scalar_type t -> t | Ast.TRef(Ast.TArray(t, _)) when Ast.is_scalar_type t -> t | _ -> ignore_pattern ()) in
            (Ast.AccIndex(lv_new_node, exp_new_node)) @> final_typ
          | {Ast.annot = _; _} -> raise (Semantic_error(loc, "The array index does not evaluate to an integer!"))
        end
    end
  | Ast.AccVar(None, var_id) ->
    let (cname, csym_tbl, cuses) = (match component_sym with SComponent({cattr = {id = cname; _}; csym_tbl; cuses; _}) -> (cname, csym_tbl, cuses) | _ -> ignore_pattern ()) in
    let (comp_uses_identifiers) = (match component_ast_node with Ast.ComponentDecl({uses; _}) -> uses) in
    let check_inside_table name sym_tbl = 
      let sym = Symbol_table.lookup var_id sym_tbl in
      let typ = (match sym with SVar({vattr = {typ; _}}) -> typ | SFunction({fattr; _}) -> raise (Semantic_error(loc, (Printf.sprintf "Cannot access function `%s.%s` as a value!" cname fattr.id))) | _ -> ignore_pattern ()) in
      (Ast.AccVar(name, var_id)) @> typ
    in
    let rec lookup_used_interfaces = function
    | [] -> raise (Semantic_error(loc, "Missing variable declaration!"))
    | iname::tail ->
        let isym_tbl = (match Symbol_table.lookup iname cuses with SInterface({isym_tbl; _}) -> isym_tbl | _ -> ignore_pattern ()) in 
        try 
          check_inside_table (Some iname) isym_tbl
        with Symbol_table.MissingEntry(_) ->
          lookup_used_interfaces tail
    in
    begin
      try
        (* If the function symbol table pointer is equal to the component symbol table, *)
        (* then we are checking an expression inside the global scope. *)
        if function_sym_tbl == csym_tbl then 
          check_inside_table (Some cname) function_sym_tbl
        else
          (* Otherwise we normally check the function symbol table. *)
          check_inside_table None function_sym_tbl
      with Symbol_table.MissingEntry(_) ->
        begin
          try
            (* Is the variable inside the component scope? *)
            check_inside_table (Some(cname)) csym_tbl
          with Symbol_table.MissingEntry(_) ->
            begin
              (* As last chance we can only search inside used interfaces. *)
              lookup_used_interfaces comp_uses_identifiers
            end
        end
    end
  | _ -> ignore_pattern ()

and type_check_expr fun_env annotated_expr = 

  let component_ast_node = fun_env.component_ast_node in 
  let component_sym = fun_env.component_sym in
  let current_symbol_table = fun_env.current_symbol_table in

  let node = annotated_expr.Ast.node in 
  let loc = annotated_expr.Ast.annot in 

  match node with

  | Ast.LV(lv) -> 
    let new_node_lv = type_check_lvalue fun_env lv in
    (Ast.LV(new_node_lv)) @> (new_node_lv.Ast.annot)

  | Ast.Assign(lv, e) ->
    let new_node_lv = type_check_lvalue fun_env lv in
    let new_node_exp = type_check_expr fun_env e in
    let type_check_res = type_check_assign new_node_lv.Ast.annot new_node_exp.Ast.annot in
    (* Check if the assignment is valid. If it is not then raise a semantic error by reading the error message. *)
    if Result.is_ok type_check_res then
      (Ast.Assign(new_node_lv, new_node_exp)) @> (new_node_lv.Ast.annot)
    else
      raise (Semantic_error(loc, Result.get_error type_check_res))

  | Ast.ILiteral(i) -> (Ast.ILiteral(i)) @> Ast.TInt
  | Ast.CLiteral(c) -> (Ast.CLiteral(c)) @> Ast.TChar
  | Ast.BLiteral(b) -> (Ast.BLiteral(b)) @> Ast.TBool
  | Ast.FLiteral(f) -> (Ast.FLiteral(f)) @> Ast.TFloat

  | Ast.UnaryOp(uop, exp) ->
    let new_node_exp = type_check_expr fun_env exp in 
    begin
      match (uop, new_node_exp.Ast.annot) with
      | (Ast.Neg, Ast.TInt)             -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TInt
      | (Ast.Neg, Ast.TFloat)           -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TFloat
      | (Ast.Neg, Ast.TRef(Ast.TInt))   -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TInt
      | (Ast.Neg, Ast.TRef(Ast.TFloat)) -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TFloat

      | (Ast.Not, Ast.TBool)            -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TBool
      | (Ast.Not, Ast.TRef(Ast.TBool))  -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TBool

      (* Error cases. Neg and Not operator applied with wrong paramater types. *)
      | (Ast.Neg, wrong_type) ->
        let msg = Printf.sprintf "Minus operator cannot be applied to %s type!" (Ast.show_type wrong_type) in
        raise (Semantic_error(loc, msg))
      | (Ast.Not, wrong_type) -> 
        let msg = Printf.sprintf "Not operator cannot be applied to %s type!" (Ast.show_type wrong_type) in
        raise (Semantic_error(loc, msg))
    end

  | Ast.DoubleOp(dop, dop_prec, lv) ->
    let new_lv_node = type_check_lvalue fun_env lv in
    begin
      match (new_lv_node.Ast.annot) with 
      | Ast.TInt    -> (Ast.DoubleOp(dop, dop_prec, new_lv_node)) @> (Ast.TInt)
      | Ast.TFloat  -> (Ast.DoubleOp(dop, dop_prec, new_lv_node)) @> (Ast.TFloat)
      | _ -> raise (Semantic_error(loc, "Increment/decrement operator can be applied only to numerical types!")) 
    end

  | Ast.Address(lv) ->
    let new_lv_node = type_check_lvalue fun_env lv in
    (Ast.Address(new_lv_node)) @> (Ast.TRef(new_lv_node.Ast.annot))

  | Ast.BinaryOp(binop, exp1, exp2) ->
    let new_node_exp1 = type_check_expr fun_env exp1 in 
    let new_node_exp2 = type_check_expr fun_env exp2 in 
    begin
      let typ1 = new_node_exp1.Ast.annot in 
      let typ2 = new_node_exp2.Ast.annot in 
      match (binop, typ1, typ2) with

      (* +, -, *, /, % operators *)
      | (_,   Ast.TInt, Ast.TInt)                         (* int + int *)
      | (_,   Ast.TFloat, Ast.TFloat)                     (* float + float *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TInt)               (* &int + int *)
      | (_,   Ast.TRef(Ast.TFloat), Ast.TFloat)           (* &float + float *)
      | (_,   Ast.TInt, Ast.TRef(Ast.TInt))               (* int + &int *)
      | (_,   Ast.TFloat, Ast.TRef(Ast.TFloat))           (* float + &float *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TRef(Ast.TInt))     (* &int + &int *)
      | (_,   Ast.TRef(Ast.TFloat), Ast.TRef(Ast.TFloat)) (* &float + &float *)
      when Ast.is_math_operator binop -> (Ast.BinaryOp(binop, new_node_exp1, new_node_exp2)) @> (typ1)
      
      (* >, <, >=, <= *)
      | (_,   Ast.TInt, Ast.TInt)                         (* int + int *)
      | (_,   Ast.TFloat, Ast.TFloat)                     (* float + float *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TInt)               (* &int + int *)
      | (_,   Ast.TRef(Ast.TFloat), Ast.TFloat)           (* &float + float *)
      | (_,   Ast.TInt, Ast.TRef(Ast.TInt))               (* int + &int *)
      | (_,   Ast.TFloat, Ast.TRef(Ast.TFloat))           (* float + &float *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TRef(Ast.TInt))     (* &int + &int *)
      | (_,   Ast.TRef(Ast.TFloat), Ast.TRef(Ast.TFloat)) (* &float + &float *)
      when Ast.is_compare_operator binop -> (Ast.BinaryOp(binop, new_node_exp1, new_node_exp2)) @> Ast.TBool
      
      (* ==, != *)
      | (Ast.Equal, _, _)
      | (Ast.Neq,   _, _) when (Ast.equal_typ typ1 typ2) && (Ast.is_scalar_type typ1 || Ast.is_ref_to_scalar_type typ1) -> (Ast.BinaryOp(binop, new_node_exp1, new_node_exp2)) @> Ast.TBool

      (* &&, || operators *)
      | (_, Ast.TBool, Ast.TBool)
      | (_, Ast.TRef(Ast.TBool), Ast.TBool)
      | (_, Ast.TBool, Ast.TRef(Ast.TBool))
      | (_, Ast.TRef(Ast.TBool), Ast.TRef(Ast.TBool)) 
      when Ast.is_bool_operator binop -> (Ast.BinaryOp(binop, new_node_exp1, new_node_exp2)) @> Ast.TBool

      (* Error cases ... *)
      | (Ast.Add, _, _) -> 
        let msg = Printf.sprintf "You cannot sum a `%s` with a `%s`!" (Ast.show_type typ1) (Ast.show_type typ2) in
        raise (Semantic_error(loc, msg))

      | (Ast.Equal, _, _)
      | (Ast.Neq, _, _) when not(Ast.equal_typ typ1 typ2) -> 
        let msg = Printf.sprintf "Invalid comparsion since the two types are different (%s <> %s)!" (Ast.show_type typ1) (Ast.show_type typ2) in 
        raise (Semantic_error(loc, msg))

      | _ -> raise (Semantic_error(loc, "Binary operation not allowed!"))
    end

  | Ast.Call(None, fname, exp_actual_list) ->
    
    (* Get the component symbol table *)
    let (comp_sym_tbl, comp_uses_sym_tbl) = (match component_sym with SComponent({csym_tbl; cuses; _}) -> (csym_tbl, cuses) | _ -> ignore_pattern ()) in
    let (cname, comp_uses_identifiers) = (match component_ast_node with Ast.ComponentDecl({cname; uses; _}) -> (cname,uses)) in

    (* Function to perform the linking phase to qualify the function call to an interface *)
    let perform_call_linking name fsym_attr = 
        let rec type_check_actuals formals actuals = 
          match (formals, actuals) with
          | ([], []) -> true
          | (h1::t1, h2::t2) when (Ast.equal_typ h1 h2) -> type_check_actuals t1 t2
          | (h1::t1, (Ast.TRef(h2))::t2) when (Ast.equal_typ h1 h2) -> type_check_actuals t1 t2
          | _ -> false
        in
        (* Evaluate the actual paramaters to get info about the types *)
        let new_exp_actual_list = List.map (fun e -> type_check_expr fun_env e) exp_actual_list in
        let actuals_type = List.map (fun x -> match x.Ast.annot with Ast.TArray(i, _) -> (Ast.TArray(i, None)) | _ -> x.Ast.annot) new_exp_actual_list in 
        match fsym_attr.typ with 
        | Ast.TFun(formals_type, rtype) ->  
          let length_formals = (List.length formals_type) in 
          let length_actuals = (List.length actuals_type) in
          (* Check if the calling function has the exact number of paramaters *)
          if (length_actuals > length_formals) || (length_actuals < length_formals) then
            let msg = Printf.sprintf "Error on invoking function `%s`. The function expceted %d parameter(s) but %d was(were) given!" fname length_formals length_actuals in
            raise (Semantic_error(loc, msg))
          else
            (* If each type of the actual match with the formals then success! *)
            if type_check_actuals formals_type actuals_type then
              (Ast.Call(name, fname, new_exp_actual_list)) @> rtype
            else
              raise (Semantic_error(loc, "The arguments type provided to the function call are wrong!\n" ))
        | _ -> ignore_pattern ()
    in

    let rec lookup_used_interfaces = function
    | [] -> raise (Semantic_error(loc, "Call to an undefined function..."))
    | iname::t ->
      if iname = Mcomp_stdlib.g_PRELUDE_ID then
        let isym_tbl = (match Symbol_table.lookup iname comp_uses_sym_tbl with SInterface({isym_tbl; _}) -> isym_tbl | _ -> ignore_pattern ()) in
        try
          let ifun_attr = (match Symbol_table.lookup fname isym_tbl with SFunction({fattr; _}) -> fattr | SVar(_) -> raise (Semantic_error(loc, "Cannot invoke a variable!")) | _ -> ignore_pattern ()) in 
          perform_call_linking (Some Mcomp_stdlib.g_PRELUDE_ID) ifun_attr
        with Symbol_table.MissingEntry(_) -> 
            lookup_used_interfaces t
      else
        let isym_tbl = (match Symbol_table.lookup iname comp_uses_sym_tbl with SInterface({isym_tbl; _}) -> isym_tbl | _ -> ignore_pattern ()) in
        try
          let ifun_attr = (match Symbol_table.lookup fname isym_tbl with SFunction({fattr; _}) -> fattr | SVar(_) -> raise (Semantic_error(loc, "Cannot invoke a variable!")) | _ -> ignore_pattern ()) in 
          perform_call_linking (Some iname) ifun_attr
        with 
        | Symbol_table.MissingEntry(_) ->
            lookup_used_interfaces t
        | Not_found ->
            let msg = Printf.sprintf "Looking for function `%s` failed, since the component `%s` does not link with any component providing the `%s` interface!" fname cname iname in
            raise (Semantic_error(loc, msg))
    in
    begin
      let aux () =
        try
          (* Search for the function name inside component symbol table*)
          let ifun_attr = (match Symbol_table.lookup fname comp_sym_tbl with SFunction({fattr; _}) -> fattr | SVar(_) -> raise (Semantic_error(loc, "Cannot invoke a variable!"))  |  _ -> ignore_pattern ()) in 
          perform_call_linking None ifun_attr
        with Symbol_table.MissingEntry(_) ->
          (* Well, the function is not defined inside our component, let's go to search it *)
          (* inside the provided interfaces... *)
          lookup_used_interfaces (Mcomp_stdlib.g_PRELUDE_ID::comp_uses_identifiers)
      in
      (* Let's be sure that we are not invoking a variable... *)
      if comp_sym_tbl == current_symbol_table then
        aux ()
      else
        try
          let _ = (match Symbol_table.lookup fname current_symbol_table with SVar(_) -> true |  _ -> ignore_pattern ()) in 
          raise (Semantic_error(loc, "Cannot invoke a variable!"))
        with Symbol_table.MissingEntry(_) ->
          (* If the name doesn't belong to a variable then we can check the function *)
          aux ()
    end
  | _ -> 
    (Printf.printf "%s\n\n" (Ast.show_expr_node (fun _ _ -> ()) node));
    ignore_pattern ()
    
and type_check_stmt fun_env fun_rtype annotated_stmt = 

  let function_sym_tbl = fun_env.current_symbol_table in
  let node = annotated_stmt.Ast.node in 
  let loc = annotated_stmt.Ast.annot in

  match node with
  | Ast.If(b_expr, then_stmt, else_stmt) ->
    let new_bexp = type_check_expr fun_env b_expr in
    let sym_table = Symbol_table.begin_block function_sym_tbl in
    let fun_env = {fun_env with current_symbol_table = sym_table} in
    let new_then_stmt = type_check_stmt fun_env fun_rtype then_stmt in
    let sym_table = Symbol_table.begin_block sym_table in
    let fun_env = {fun_env with current_symbol_table = sym_table} in
    let new_else_stmt = type_check_stmt fun_env fun_rtype else_stmt in
    begin
      let etyp = new_bexp.Ast.annot in 
      match etyp with 
      | Ast.TBool -> (Ast.If(new_bexp, new_then_stmt, new_else_stmt)) @> Ast.TVoid 
      | _ -> raise (Semantic_error(loc, "The if guard does not evaluate to a boolean type!"))
    end

  | Ast.While(b_expr, wstmt) ->
      let new_bexp = type_check_expr fun_env b_expr in 
      let sym_table = Symbol_table.begin_block function_sym_tbl in
      let fun_env = {fun_env with current_symbol_table = sym_table} in
      let new_wstmt = type_check_stmt fun_env fun_rtype wstmt in 
      begin
        let etyp = new_bexp.Ast.annot in 
        match etyp with
        | Ast.TBool -> (Ast.While(new_bexp, new_wstmt)) @> Ast.TVoid
        | _ -> raise (Semantic_error(loc, "The while guard does not evaluate to a boolean type!"))
      end

  | Ast.Expr(e1) ->
    let new_exp = type_check_expr fun_env e1 in
    let etyp = new_exp.Ast.annot in 
    (Ast.Expr(new_exp)) @> etyp

  | Ast.For(e1, e2, e3, for_stmt) ->
    let sym_table = Symbol_table.begin_block function_sym_tbl in
    let fun_env = {fun_env with current_symbol_table = sym_table} in
    let new_for_stmt = type_check_stmt fun_env fun_rtype for_stmt in
    let new_e1 = match e1 with Some(e1) -> Some (type_check_expr fun_env e1) | None -> None in
    let new_e2 = match e2 with Some(e2) -> (type_check_expr fun_env e2) | None -> (Ast.BLiteral(true) @> Ast.TBool) in
    let new_e3 = match e3 with Some(e3) -> Some (type_check_expr fun_env e3) | None -> None in
    begin
      (* Check if the second expression evaluates to a boolean one *)
      match new_e2.Ast.annot with
      | Ast.TBool -> Ast.For(new_e1, Some new_e2, new_e3, new_for_stmt) @> (Ast.TVoid)
      | _ -> raise (Semantic_error(loc, "The for guard is not a boolean expression!"))
    end
  
  | Ast.Return(None) -> (Ast.Return(None)) @> Ast.TVoid

  | Ast.Return(Some(exp)) ->
      let new_exp = type_check_expr fun_env exp in 
      (* Let's check if the expression type matches with the function return type *)
      if Ast.equal_typ (new_exp.Ast.annot) (fun_rtype) then
        (Ast.Return(Some(new_exp))) @> (new_exp.Ast.annot)
      else
        let msg = Printf.sprintf "The expression type returned by the function does not match the return type.\nAn `%s` expression was expected, got `%s` instead!" (Ast.show_type fun_rtype) (Ast.show_type (new_exp.Ast.annot)) in
        raise (Semantic_error(loc, msg))
  
  | Ast.Block(stmts) ->
    (* Create a new block inside the symbol table *)
    let type_check_stmtordec annotated_node (fun_rtype, sym_table) =
      match (annotated_node.Ast.node) with
      | Ast.LocalDecl((_, typ) as decl, Some exp) ->
        let _ = check_local_decl_type annotated_node in
        let new_exp = type_check_expr fun_env exp in
        (* Let's ensure that expression type is the same as the variable declaration *)
        let res_type_check = type_check_assign typ new_exp.Ast.annot in
        if Result.is_ok res_type_check then
          (Ast.LocalDecl(decl, Some new_exp)) @> (typ)
        else
          raise (Semantic_error(loc, Result.get_error res_type_check))

      | Ast.LocalDecl(vdecl, None) -> 
        let _ = check_local_decl_type annotated_node in 
        (Ast.LocalDecl(vdecl, None)) @> (snd vdecl)

      | Ast.Stmt(stmt) ->
        let fun_env = {fun_env with current_symbol_table = sym_table} in
        let new_stmt = type_check_stmt fun_env fun_rtype stmt in 
        (Ast.Stmt(new_stmt)) @> Ast.TVoid
    in
    let aux_fill_local_st sym_tbl annotated_node =
      let n = annotated_node.Ast.node in 
      let l = annotated_node.Ast.annot in
      match n with
      | Ast.LocalDecl((id, typ), _) -> 
        begin
          try
            Symbol_table.add_entry id (SVar({vattr = {id = id; loc = l; typ = typ}})) sym_tbl
          with Symbol_table.DuplicateEntry(_) ->
            let msg = Printf.sprintf "The variable %s has been declared twice!" id in 
            raise (Semantic_error(loc, msg))
        end
      | _ -> sym_tbl 
    in

    let (_, new_stmts) = List.fold_left (fun (st, statements) stmt -> 
      let st = aux_fill_local_st st stmt in
      let stmt = type_check_stmtordec stmt (fun_rtype, st) in
      (st, stmt::statements)
    ) ((Symbol_table.begin_block function_sym_tbl), []) stmts in
    
    (Ast.Block(List.rev (new_stmts))) @> Ast.TVoid

  | Ast.Skip -> (Ast.Skip) @> Ast.TVoid

(** The function performs type checking *)
let second_pass ast global_table =

  let visit_component_member ast_node local_sym member_node =
    let node = member_node.Ast.node in 
    let loc = member_node.Ast.annot in
    match node with
    | Ast.VarDecl(vd, None) -> (Ast.VarDecl(vd, None)) @> (snd vd)

    | Ast.VarDecl((_, vtyp) as decl, Some exp) -> 
      let csym_tbl = (match local_sym with SComponent({csym_tbl; _}) -> csym_tbl | _ -> ignore_pattern ()) in
      let fun_env = {
        component_ast_node = ast_node;
        component_sym = local_sym;
        current_symbol_table = csym_tbl;
      } in
      let new_exp = type_check_expr fun_env exp in
      let res_type_check = type_check_assign vtyp new_exp.Ast.annot in
      if Result.is_ok res_type_check then
        (Ast.VarDecl(decl, Some new_exp)) @> (vtyp)
      else
        raise (Semantic_error(loc, Result.get_error res_type_check))

    | Ast.FunDecl({Ast.body = None; _}) -> ignore_pattern () (* The component functions have a body. *)

    | Ast.FunDecl({Ast.rtype; Ast.fname; Ast.formals; body = Some(fbody)}) ->
      let _ = check_fun_return_type member_node in
      let csym_tbl = (match local_sym with SComponent({csym_tbl; _}) -> csym_tbl | _ -> ignore_pattern ()) in
      let fn_sym = Symbol_table.lookup fname csym_tbl in
      let fn_st = (match fn_sym with SFunction({fsym_tbl; _}) -> fsym_tbl | _ -> ignore_pattern ()) in
      let fun_env = {
        component_ast_node = ast_node;
        component_sym = local_sym;
        current_symbol_table = fn_st;
      } in
      let new_body = type_check_stmt fun_env rtype fbody in
      let formals_type = List.map (snd) formals in
      (Ast.FunDecl({Ast.rtype = rtype; fname = fname; formals = formals; body = Some(new_body)})) @> (Ast.TFun(formals_type, rtype))
  in
  let visit_interface_member member_node =
    let node = member_node.Ast.node in 
    match node with
    | Ast.VarDecl(vd, None) -> (Ast.VarDecl(vd, None)) @> (snd vd)
    | Ast.FunDecl({Ast.rtype; Ast.fname; Ast.formals; body = None}) -> 
      let formals_type = List.map (snd) formals in
      (Ast.FunDecl({Ast.rtype = rtype; fname = fname; formals = formals; body = None})) @> (Ast.TFun(formals_type, rtype))
    | Ast.VarDecl(_, _) -> ignore_pattern () (* Inline variable declaration is not allowed inside interfaces *)
    | Ast.FunDecl(_) -> ignore_pattern () (* Interface functions don't have a body! *)
  in
  let visit_component comp_node = 
    let node = comp_node.Ast.node in
    match node with
    | Ast.ComponentDecl({cname = cname; uses = uses; provides = provides; definitions = definitions; }) ->
      let csym = Symbol_table.lookup cname global_table in
      let new_definitions = List.map (fun x -> visit_component_member node csym x) definitions in 
      let ctyp = Ast.TComponent(cname) in 
      (Ast.ComponentDecl({cname = cname; uses = uses; provides = provides; definitions = new_definitions})) @> ctyp
  in
  let visit_interface iface_node = 
    let node = iface_node.Ast.node in
    match node with
    | Ast.InterfaceDecl({iname = iname; declarations = declarations; }) ->
      let new_declarations = List.map (fun x -> visit_interface_member x) declarations in 
      let ityp = Ast.TInterface(iname) in 
      (Ast.InterfaceDecl({iname = iname; declarations = new_declarations})) @> ityp
  in
  let (interfaces, components, connections) = ( match ast with Ast.CompilationUnit(c) -> (c.interfaces, c.components, c.connections)) in
  let interfaces = List.map visit_interface interfaces in 
  let components = List.map visit_component components in 
  (Ast.CompilationUnit{interfaces = interfaces; components = components; connections = connections})

let type_check ast = 
  let global_table = Symbol_table.begin_block (Symbol_table.empty_table) in
  first_pass ast global_table |> second_pass ast