exception Semantic_error of Location.code_pos * string

module StrMap = Map.Make(String)

type attr = {id: Ast.identifier; loc: Location.code_pos; typ: Ast.typ}
type 'a sym = 
  | SComponent of {cattr: 'a; csym_tbl: 'a sym_table; cprov: 'a sym_table; cuses: 'a sym_table; cconnect: (string) StrMap.t}
  | SInterface of {iattr: 'a; isym_tbl: 'a sym_table}
  | SFunction  of {fattr: 'a; fsym_tbl: 'a sym_table}
  | SVar       of {vattr: 'a}
and 'a sym_table = ('a sym) Symbol_table.t 

let (@>) node annot = Ast.make_node node annot

(** Function used inside pattern matching for cases that are not eligible *)
let ignore () = failwith "Should not happen"

(** The function performs semantic-rule checks as described by the specification. *)
let first_pass ast global_table = 
  let load_prelude_interface prelude_name prelude_list global_table =
    let prelude_list = List.map (
      fun (fname, ftyp) ->
        let fattr = {id = fname; loc = Location.dummy_code_pos; typ = ftyp} in 
        let fsym = SFunction({fattr = fattr; fsym_tbl = Symbol_table.empty_table}) in
        (fname, fsym)
    ) prelude_list in
    let iattr = {id = prelude_name; loc = Location.dummy_code_pos; typ = Ast.TInterface(prelude_name)} in 
    let isym_tbl = Symbol_table.of_alist prelude_list in 
    let isym = SInterface({iattr = iattr; isym_tbl}) in
    Symbol_table.add_entry prelude_name isym global_table
  in
  let rec visit_interface_declarations isym_tbl iname = function
  | [] -> isym_tbl
  | annotated_node::tail ->
      let node = annotated_node.Ast.node in 
      let loc = annotated_node.Ast.annot in 
      match node with 
      | Ast.FunDecl({Ast.rtype; fname; formals; _}) ->
        let formals_types = List.map (fun (_, t) -> t) formals in 
        let fattr = {id = fname; loc = loc; typ = Ast.TFun(formals_types, rtype)} in
        let fsym_tbl = Symbol_table.empty_table in
        let fsym = SFunction({fattr = fattr; fsym_tbl = fsym_tbl}) in
        begin
          try
            visit_interface_declarations (Symbol_table.add_entry fname fsym isym_tbl) iname tail 
          with Symbol_table.DuplicateEntry(_) ->
            let msg = Printf.sprintf "Double identifier `%s` found inside `%s` interface members" fname iname in
            raise (Semantic_error(loc, msg))
        end
      | Ast.VarDecl(vid, vtyp) ->
        let vattr = {id = vid; loc = loc; typ = vtyp} in
        let vsym = SVar({vattr = vattr}) in
        begin
          try
            visit_interface_declarations (Symbol_table.add_entry vid vsym isym_tbl) iname tail
          with Symbol_table.DuplicateEntry(_) ->
            let msg = Printf.sprintf "Double identifier `%s` found inside `%s` interface members" vid iname in
            raise (Semantic_error(loc, msg))
        end
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
            | _ -> ignore ()
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
            (* TODO: type checking on formals *)
            if Ast.equal_typ vtyp Ast.TVoid then
              raise (Semantic_error(loc, "The formal parameter cannot be a void type!"))
            else
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
          (* Let's check prelude function... *)
          List.iter (fun (name, _) -> if (name = fname) then raise (Semantic_error(loc, "Cannot define the function since it is contained in the prelude!")) else ()) Mcomp_stdlib.prelude_signature;
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
        | Ast.VarDecl(vid, vtyp) ->
          begin
            match vtyp with
            | Ast.TVoid -> raise (Semantic_error(loc, "Variables cannot be declared of type void"))
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
      (* Create components attribute... *)
      let cattr = {id = cname; loc = loc; typ = Ast.TComponent(cname)} in

      (* Check if the provides list has the prelude interface *)
      if List.mem "Prelude" provides then
        raise (Semantic_error(loc, "The Prelude interface cannot be provided by any component!"))
      else
        (* Create a symbol table containing interface references provided by the component *)
        let cprov = Symbol_table.begin_block (Symbol_table.empty_table) in 
        let cprov = visit_prov_uses cprov "provides" provides in
        (* Create a symbol table containing interface references provided by the component *)
        let cuses = Symbol_table.begin_block (Symbol_table.empty_table) in
        let cuses = visit_prov_uses cuses "uses" (List.cons "Prelude" uses) in
        (* 
          A component must implement all the members defined in the interfaces it provides,
          so from the defined_interfaces map filter the interfaces provided by the component...
        *)
        (* Build component symbol table containing all the definitions inside a component (fields/functions) *)
        let csym_tbl = Symbol_table.begin_block (Symbol_table.empty_table) in 
        let csym_tbl = visit_member_definitions csym_tbl definitions in
        (* Create a new symbol for the component and push it inside the symbol table *)
        let csym = SComponent({cattr = cattr; csym_tbl = csym_tbl; cprov = cprov; cuses = cuses; cconnect = StrMap.empty}) in
        try
          visit_components (Symbol_table.add_entry cname csym global_table) tail
        with Symbol_table.DuplicateEntry(_) ->
          (* Check for duplicated symbol identifier *)
          begin
            let dup_sym = Symbol_table.lookup cname global_table in (* A MissingEntry exception cannot be thrown since the name is contained inside the table already *)
            match dup_sym with 
            | SInterface(_) -> 
              let msg = Printf.sprintf "Component name not valid since it seems that an interface named` %s` already exists." cname in
              raise (Semantic_error(loc, msg))
            | SComponent(_) ->
              let msg = Printf.sprintf "Duplicated component name. It seems that a component named` %s` already exists." cname in
              raise (Semantic_error(loc, msg))
            | _ -> ignore ()
          end
  in
  let rec check_component_provides global_table interfaces = function
  | [] -> global_table
  | annotated_node::tail -> 
    let node = annotated_node.Ast.node in 
    let loc = annotated_node.Ast.annot in
    match node with
    | Ast.ComponentDecl({cname; provides; _}) ->
      let rec find_interface name = function
      | [] -> None
      | ({Ast.node = (Ast.InterfaceDecl({iname; _}) as iface); _})::tail -> if iname = name then Some iface else find_interface name tail 
      in
      let cysm_tbl = (match Symbol_table.lookup cname global_table with SComponent({csym_tbl; _}) -> csym_tbl | _ -> ignore ()) in 
      let interfaces_nodes = List.map (fun iname -> find_interface iname interfaces) provides in
      let _ = List.iter(fun i ->
        match i with 
        | None -> ()
        | Some iface ->
        let (iname, ideclarations) = (match (iface) with Ast.InterfaceDecl({iname; declarations}) -> (iname, declarations)) in
        let isym_tbl = (match Symbol_table.lookup iname global_table with SInterface({isym_tbl; _}) -> isym_tbl | _ -> ignore ()) in
        List.iter (fun imember -> 
          let imember_node = imember.Ast.node in 
          match imember_node with
          | Ast.FunDecl({Ast.fname; _}) ->
            let ifsym = Symbol_table.lookup fname isym_tbl in
            begin
              try
                let cfsym = Symbol_table.lookup fname cysm_tbl in
                match (ifsym, cfsym) with
                | (SFunction({fattr = {typ = iftyp; _}; _}), SFunction({fattr = {typ = cityp; loc = cloc; _}; _})) -> 
                  if not(Ast.equal_typ iftyp cityp) then
                    let msg = Printf.sprintf "The function `%s.%s` type mismatch with the one defined in the interface `%s`!" cname fname iname in 
                    raise (Semantic_error(cloc, msg))
                | (_, _) -> ignore ()
              with Symbol_table.MissingEntry(_) -> 
                let msg = Printf.sprintf "The function `%s` defined inside the interface `%s` was not implemented inside the component `%s`!" fname iname cname in
                raise (Semantic_error(loc, msg))
            end
          | Ast.VarDecl(ivid, _) ->
            let ivsym = Symbol_table.lookup ivid isym_tbl in
            begin
              try
                let cvsym = Symbol_table.lookup ivid cysm_tbl in
                match (ivsym, cvsym) with
                | (SVar({vattr = {typ = ivtyp; _}}), SVar({vattr = {typ = cvtyp; loc = cloc; _}})) -> 
                  if not(Ast.equal_typ ivtyp cvtyp) then
                    let msg = Printf.sprintf "The field `%s.%s` type mismatch with the one defined in the interface `%s`!" cname ivid iname in 
                    raise (Semantic_error(cloc, msg))
                | (_, _) -> ignore ()
              with Symbol_table.MissingEntry(_) -> 
                let msg = Printf.sprintf "The field `%s` defined inside the interface `%s` was not implemented inside the component `%s`!" ivid iname cname in
                raise (Semantic_error(loc, msg))
            end
        ) ideclarations
      ) interfaces_nodes in
      check_component_provides global_table interfaces tail
  in
  let rec check_main_components global_table app_provided = function
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
      (* does the component provide the App interface? *)
      let temp = List.exists ((=) "App") provides in
      if app_provided && temp then
        let msg = "There was already a component providing the `App` interface." in
        raise (Semantic_error(loc, msg))
      else
        check_main_components global_table (app_provided || temp) tail
  in
  let rec link_connect_block global_table = function
  | [] -> global_table
  | (Ast.Link(c1, _, c2, _))::_ when (c1 = "Prelude") || (c2 = "Prelude") ->
    raise (Semantic_error(Location.dummy_code_pos, "Link to Prelude interface cannot be specified!"))
  | (Ast.Link(c1, i1, c2, i2))::tail -> 
    begin
      try 
        let c1_sym = (match Symbol_table.lookup c1 global_table with 
          | SComponent(_) as c -> c 
          | SInterface({iattr = {id; _}; _}) -> raise (Semantic_error(Location.dummy_code_pos, (Printf.sprintf "Link invalid since %s is an interface!" id)))
          | _ -> ignore ()) in 
        let c2_sym = (match Symbol_table.lookup c2 global_table with 
          | SComponent(_) as c -> c 
          | SInterface({iattr = {id; _}; _}) -> raise (Semantic_error(Location.dummy_code_pos, (Printf.sprintf "Link invalid since %s is an interface!" id)))
          | _ -> ignore ()) in 
        begin
          try
            let (c1_sym_cuses, c1_sym_cconect) = (match c1_sym with SComponent({cuses; cconnect; _}) -> (cuses, cconnect) | _ -> ignore ()) in
            let c2_sym_cprov = (match c2_sym with SComponent({cprov;_}) -> cprov | _ -> ignore ()) in
            let _ = Symbol_table.lookup i1 c1_sym_cuses in
            let _ = Symbol_table.lookup i2 c2_sym_cprov in
            let new_cconnect = StrMap.add i1 c2 c1_sym_cconect in
            let c1_sym_updated = (match c1_sym with SComponent(c) -> SComponent({c with cconnect = new_cconnect}) | _ -> ignore ()) in
            link_connect_block (Symbol_table.update_entry c1 c1_sym_updated global_table) tail
          with Symbol_table.MissingEntry(missing) ->
            let msg = Printf.sprintf "Cannot find the declaration for interface `%s`!" missing in
            raise (Semantic_error(Location.dummy_code_pos, msg))
        end
      with Symbol_table.MissingEntry(missing) ->
        let msg = Printf.sprintf "Cannot find the decleration for component `%s`!" missing in 
        raise (Semantic_error(Location.dummy_code_pos, msg))
    end
  in
  let (interfaces, components, connections) = (match ast with Ast.CompilationUnit({interfaces; components; connections}) -> (interfaces, components, connections)) in
  (* Based the global table table containing the interface symbols, let's get a string map that will be used later with components *)
  (* Preload the `Prelude` and `App` interface into the global symbol table *)
  let global_table = load_prelude_interface "Prelude" Mcomp_stdlib.prelude_signature global_table in 
  let global_table = load_prelude_interface "App" Mcomp_stdlib.app_signature global_table in 
  (* Load the interface definitions *)
  let global_table = visit_interfaces global_table interfaces in
  let global_table = visit_components global_table components in
  (* Perform some checks *)
  let global_table = check_component_provides global_table interfaces components in
  let global_table = check_main_components global_table false components in
  (* At the end, perform linking *)
  let global_table = link_connect_block global_table connections in
  global_table


let _check_local_decl_type annotated_node = 
  let node = annotated_node.Ast.node in 
  let loc = annotated_node.Ast.annot in 
  match node with 
  | Ast.LocalDecl(_, Ast.TVoid) -> raise (Semantic_error(loc, "Cannot declare variables of type void!"))
  | Ast.LocalDecl(_, Ast.TArray(_, Some n)) when n <= 0 -> raise (Semantic_error(loc, "The array size cannot be less than 0!"))
  | Ast.LocalDecl(_, Ast.TArray(t, _)) when not(Ast.is_scalar_type t) -> raise (Semantic_error(loc, "Only arrays of scalar types are allowed!"))
  | Ast.LocalDecl(_, _) -> ()
  | Ast.Stmt(_) -> ignore ()


let _check_fun_rtype annotated_node = 
  let node = annotated_node.Ast.node in
  let loc = annotated_node.Ast.annot in 
  match node with
  | Ast.FunDecl({Ast.rtype = rtype; _}) -> 
    begin
      match rtype with 
      | Ast.TInt | Ast.TBool | Ast.TChar | Ast.TVoid -> ()
      | _  -> raise (Semantic_error(loc, "A function can only returns int, bool, char or void."))
    end
  | Ast.VarDecl(_) -> ignore ()


let rec _type_check_lvalue component_ast_node component_sym function_sym_tbl annotated_lv = 
  let node = annotated_lv.Ast.node in 
  let loc = annotated_lv.Ast.annot in 
  match node with
  | Ast.AccIndex(lv, exp) ->
    let lv_new_node = _type_check_lvalue component_ast_node component_sym function_sym_tbl lv in 
    begin
      match (lv_new_node) with
      | {Ast.node = Ast.AccIndex(_, _); _} -> ignore ()
      | {Ast.node = Ast.AccVar(_, _); annot = lv_typ} -> 
        let exp_new_node = _type_check_expr component_ast_node component_sym function_sym_tbl exp in 
        (* Let's ensure exp_new_node has a TInt type *)
        begin
          match (exp_new_node) with 
          | {Ast.annot = Ast.TInt; _} -> 
            let final_typ = (match lv_typ with Ast.TArray(t, _) -> t | Ast.TRef(t) when Ast.is_scalar_type t -> t | Ast.TRef(Ast.TArray(t, _)) when Ast.is_scalar_type t -> t | _ -> ignore ()) in
            (Ast.AccIndex(lv_new_node, exp_new_node)) @> final_typ
          | {Ast.annot = _; _} -> raise (Semantic_error(loc, "The array index does not evaluate to an integer!"))
        end
    end
  | Ast.AccVar(None, var_id) ->
    let (cname, csym_tbl) = (match component_sym with SComponent({cattr = {id = cname; _}; csym_tbl; _}) -> (cname, csym_tbl) | _ -> ignore ()) in
    let check_inside_table name sym_tbl = 
      let sym = Symbol_table.lookup var_id sym_tbl in
      let typ = (match sym with SVar({vattr = {typ; _}}) -> typ | SFunction({fattr; _}) -> raise (Semantic_error(loc, (Printf.sprintf "Cannot access function `%s.%s` as a value!" cname fattr.id))) | _ -> failwith "Should not happen") in
      (Ast.AccVar(name, var_id)) @> typ
    in
    begin
      try
        check_inside_table None function_sym_tbl
      with Symbol_table.MissingEntry(_) ->
        begin
          try
            check_inside_table (Some(cname)) csym_tbl
          with Symbol_table.MissingEntry(_) ->
            raise (Semantic_error(loc, "Missing variable definition!"))
        end
    end
  | _ -> ignore ()
and _type_check_expr component_ast_node component_sym function_sym_tbl annotated_expr = 
  let node = annotated_expr.Ast.node in 
  let loc = annotated_expr.Ast.annot in 
  match node with
  | Ast.LV(lv) -> 
    let new_node_lv = _type_check_lvalue component_ast_node component_sym function_sym_tbl lv in
    begin
      match (new_node_lv) with
      | {Ast.annot = typ; _} -> (Ast.LV(new_node_lv)) @> typ
    end
  | Ast.Assign(lv, e) ->
    let _new_node_lv = _type_check_lvalue component_ast_node component_sym function_sym_tbl lv in
    let _new_node_exp = _type_check_expr component_ast_node component_sym function_sym_tbl e in
    Printf.printf "LV[%s] = EXP[%s]\n" (Ast.show_typ _new_node_lv.Ast.annot) (Ast.show_typ _new_node_exp.Ast.annot);
    (* Main body of type checking *)
    begin
      match (_new_node_lv, _new_node_exp) with 
      (* Case: T1 <- T2 <== T1=T2 && Scalar(T1) *)
      | ({Ast.annot = t1; _}, {Ast.annot = t2; _}) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) -> (Ast.Assign(_new_node_lv, _new_node_exp)) @> Ast.TInt
      (* Case: T1 <- &T1 <== Scalar(T1) *)
      | ({Ast.annot = t1; _}, {Ast.annot = Ast.TRef(t2); _}) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) -> (Ast.Assign(_new_node_lv, _new_node_exp)) @> Ast.TInt
      (* Case: &T1 <- &T2 <== T1==T2 && Scalar(T1) *)
      | ({Ast.annot = Ast.TRef(t1); _}, {Ast.annot = Ast.TRef(t2); _}) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) -> (Ast.Assign(_new_node_lv, _new_node_exp)) @> Ast.TInt
      (* Case: &T1 <- T2 <== T1==T2 && Scalar(T1) *)
      | ({Ast.annot = Ast.TRef(t1); _}, {Ast.annot = t2; _}) when (Ast.equal_typ t1 t2) && (Ast.is_scalar_type t1) -> (Ast.Assign(_new_node_lv, _new_node_exp)) @> Ast.TInt
      
      (* Error Cases *)
      | ({Ast.annot = Ast.TArray(_); _}, {Ast.annot = Ast.TArray(_); _}) -> raise (Semantic_error(loc, "Arrays cannot be assigned!"))
      | ({Ast.annot = _; _}, {Ast.annot = Ast.TVoid; _}) -> raise (Semantic_error(loc, "Cannot assign void to a variable!"))
      | ({Ast.annot = t1; _}, {Ast.annot = t2; _}) -> raise (Semantic_error(loc, (Printf.sprintf "Assignment not allowed! %s vs %s" (Ast.show_typ t1) (Ast.show_typ t2) )))
    end
  | Ast.ILiteral(i) -> (Ast.ILiteral(i)) @> Ast.TInt
  | Ast.CLiteral(c) -> (Ast.CLiteral(c)) @> Ast.TChar
  | Ast.BLiteral(b) -> (Ast.BLiteral(b)) @> Ast.TBool
  | Ast.UnaryOp(uop, exp) ->
    let new_node_exp = _type_check_expr component_ast_node component_sym function_sym_tbl exp in 
    begin
      match (uop, new_node_exp.Ast.annot) with
      | (Ast.Neg, Ast.TInt) -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TInt
      | (Ast.Neg, Ast.TRef(Ast.TInt)) -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TInt
      | (Ast.Neg, Ast.TChar) -> raise (Semantic_error(loc, "Minus operator cannot be applied to a character!"))
      | (Ast.Neg, Ast.TBool) -> raise (Semantic_error(loc, "Minus operator cannot be applied to a boolean!"))
      | (Ast.Not, Ast.TBool) -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TBool
      | (Ast.Not, Ast.TRef(Ast.TBool)) -> (Ast.UnaryOp(uop, new_node_exp)) @> Ast.TBool
      | (Ast.Not, Ast.TChar) -> raise (Semantic_error(loc, "Not operator cannot be applied to a character!"))
      | (Ast.Not, Ast.TInt) -> raise (Semantic_error(loc, "Not operator cannot be applied to an integer!"))
      | _ -> raise (Semantic_error(loc, "Invalid unary operator expression!"))
    end
  | Ast.DoubleOp(dop, dop_prec, lv) ->
    let new_lv_node = _type_check_lvalue component_ast_node component_sym function_sym_tbl lv in
    begin
      match (new_lv_node.Ast.annot) with 
      | Ast.TInt -> (Ast.DoubleOp(dop, dop_prec, new_lv_node)) @> (Ast.TInt)
      | _ -> raise (Semantic_error(loc, "Increment/decrement operator can be applied only to integer variables!")) 
    end
  | Ast.Address(lv) ->
    let new_lv_node = _type_check_lvalue component_ast_node component_sym function_sym_tbl lv in
    (Ast.Address(new_lv_node)) @> (Ast.TRef(new_lv_node.Ast.annot))
  | Ast.BinaryOp(binop, exp1, exp2) ->
    let new_node_exp1 = _type_check_expr component_ast_node component_sym function_sym_tbl exp1 in 
    let new_node_exp2 = _type_check_expr component_ast_node component_sym function_sym_tbl exp2 in 
    begin
      let typ1 = new_node_exp1.Ast.annot in 
      let typ2 = new_node_exp2.Ast.annot in 
      match (binop, typ1, typ2) with

      (* +, -, *, /, % operators *)
      | (_,   Ast.TInt, Ast.TInt)                     (* int + int *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TInt)           (* &int + int *)
      | (_,   Ast.TInt, Ast.TRef(Ast.TInt))           (* int + &int *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TRef(Ast.TInt)) (* &int + &int *)
      when Ast.is_math_operator binop -> (Ast.BinaryOp(binop, new_node_exp1, new_node_exp2)) @> Ast.TInt
      
      (* >, <, >=, <= *)
      | (_,   Ast.TInt, Ast.TInt)                     (* int + int *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TInt)           (* &int + int *)
      | (_,   Ast.TInt, Ast.TRef(Ast.TInt))           (* int + &int *)
      | (_,   Ast.TRef(Ast.TInt), Ast.TRef(Ast.TInt)) (* &int + &int *)
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
    let (comp_sym_tbl, comp_uses_sym_tbl, cconnect) = (match component_sym with SComponent({csym_tbl; cuses; cconnect; _}) -> (csym_tbl, cuses, cconnect) | _ -> ignore ()) in
    let (cname, comp_uses_identifiers) = (match component_ast_node with Ast.ComponentDecl({cname; uses; _}) -> (cname,uses)) in
    let perform_call_type_checking cname fsym_attr = 
        let new_exp_actual_list = List.map (fun x -> _type_check_expr component_ast_node component_sym function_sym_tbl x) exp_actual_list in
        let actuals_type = List.map (fun x -> match x.Ast.annot with Ast.TArray(i, _) -> Ast.TArray(i, None) | Ast.TRef(t) -> t | _ -> x.Ast.annot) new_exp_actual_list in 
        match fsym_attr.typ with 
        | Ast.TFun(formals_type, rtype) ->
          let length_formals = (List.length formals_type) in 
          let length_actuals = (List.length actuals_type) in
          if (length_actuals > length_formals) || (length_actuals < length_formals) then
            let msg = Printf.sprintf "Error on invoking function `%s`. The function expceted %d parameter(s) but %d was(were) given!" fname length_formals length_actuals in
            raise (Semantic_error(loc, msg))
          else
            if List.equal Ast.equal_typ formals_type actuals_type then
              (Ast.Call(Some cname, fname, new_exp_actual_list)) @> rtype
            else
              let _ = Printf.printf "Expected: " in let _ = List.iter (fun x -> Printf.printf "%s\n" (Ast.show_typ x)) formals_type in
              let _ = Printf.printf "Actual: " in let _ = List.iter (fun x -> Printf.printf "%s\n" (Ast.show_typ x)) actuals_type in
              raise (Semantic_error(loc, "The arguments type provided to the function call are wrong!\n" ))
        | _ -> ignore ()
    in
    let rec lookup_provided_interfaces = function
    | [] -> raise (Semantic_error(loc, "Call to an undefined function..."))
    | iname::t ->
      if iname = "Prelude" then
        let isym_tbl = (match Symbol_table.lookup iname comp_uses_sym_tbl with SInterface({isym_tbl; _}) -> isym_tbl | _ -> ignore ()) in
        try
          let ifun_attr = (match Symbol_table.lookup fname isym_tbl with SFunction({fattr; _}) -> fattr | SVar(_) -> raise (Semantic_error(loc, "Cannot invoke a variable!")) | _ -> ignore ()) in 
          perform_call_type_checking "Prelude" ifun_attr
        with Symbol_table.MissingEntry(_) -> 
            lookup_provided_interfaces t
      else
        let isym_tbl = (match Symbol_table.lookup iname comp_uses_sym_tbl with SInterface({isym_tbl; _}) -> isym_tbl | _ -> ignore ()) in
        try
          let ifun_attr = (match Symbol_table.lookup fname isym_tbl with SFunction({fattr; _}) -> fattr | SVar(_) -> raise (Semantic_error(loc, "Cannot invoke a variable!")) | _ -> ignore ()) in 
          (* From cconnect field derive the component name *)
          let cname = StrMap.find iname cconnect in 
          perform_call_type_checking cname ifun_attr
        with 
        | Symbol_table.MissingEntry(_) ->
            lookup_provided_interfaces t
        | Not_found ->
            let msg = Printf.sprintf "Looking for function `%s` failed, since the component `%s` does not link with any component providing the `%s` interface!" fname cname iname in
            raise (Semantic_error(loc, msg))
    in
    begin
      (* Let's be sure that we are not invoking a variable... *)
      try
        let _ = (match Symbol_table.lookup fname function_sym_tbl with SVar({vattr;}) -> vattr |  _ -> ignore ()) in 
        raise (Semantic_error(loc, "Cannot invoke a variable!"))
      with Symbol_table.MissingEntry(_) ->
        (* If the name doesn't belong to a variable then we can check the function *)
        try
          (* Search for the function name inside component symbol table*)
          let ifun_attr = (match Symbol_table.lookup fname comp_sym_tbl with SFunction({fattr; _}) -> fattr | SVar(_) -> raise (Semantic_error(loc, "Cannot invoke a variable!"))  |  _ -> ignore ()) in 
          perform_call_type_checking cname ifun_attr
        with Symbol_table.MissingEntry(_) ->
          (* Well, the function is not defined inside our component, let's go to search it *)
          (* inside the provided interfaces... *)
          lookup_provided_interfaces ("Prelude"::comp_uses_identifiers)
    end
  | _ -> ignore ()
and _type_check_stmt component_ast_node component_sym (fun_rtype, function_sym_tbl) annotated_stmt = 
  let node = annotated_stmt.Ast.node in 
  let loc = annotated_stmt.Ast.annot in
  match node with
  | Ast.If(b_expr, then_stmt, else_stmt) ->
    let new_bexp = _type_check_expr component_ast_node component_sym function_sym_tbl b_expr in
    let sym_table = Symbol_table.begin_block function_sym_tbl in
    let new_then_stmt = _type_check_stmt component_ast_node component_sym (fun_rtype, sym_table) then_stmt in 
    let sym_table = Symbol_table.end_block sym_table in 
    let sym_table = Symbol_table.begin_block sym_table in
    let new_else_stmt = _type_check_stmt component_ast_node component_sym (fun_rtype, sym_table) else_stmt in
    begin
      let etyp = new_bexp.Ast.annot in 
      match etyp with 
      | Ast.TBool -> (Ast.If(new_bexp, new_then_stmt, new_else_stmt)) @> Ast.TVoid 
      | _ -> raise (Semantic_error(loc, "The if guard does not evaluate to a boolean type!"))
    end
  | Ast.While(b_expr, wstmt) ->
      let new_bexp = _type_check_expr component_ast_node component_sym function_sym_tbl b_expr in 
      let sym_table = Symbol_table.begin_block function_sym_tbl in
      let new_wstmt = _type_check_stmt component_ast_node component_sym (fun_rtype, sym_table) wstmt in 
      begin
        let etyp = new_bexp.Ast.annot in 
        match etyp with
        | Ast.TBool -> (Ast.While(new_bexp, new_wstmt)) @> Ast.TVoid
        | _ -> raise (Semantic_error(loc, "The while guard does not evaluate to a boolean type!"))
      end
  | Ast.Expr(e1) ->
    let new_exp = _type_check_expr component_ast_node component_sym function_sym_tbl e1 in
    let etyp = new_exp.Ast.annot in 
    (Ast.Expr(new_exp)) @> etyp
  | Ast.For(e1, e2, e3, for_stmt) ->
    let sym_table = Symbol_table.begin_block function_sym_tbl in
    let new_for_stmt = _type_check_stmt component_ast_node component_sym (fun_rtype, sym_table) for_stmt in
    begin
      match (e1, e2, e3) with
      | (Some(e1), Some(e2), Some(e3)) ->
        let new_e1 = _type_check_expr component_ast_node component_sym function_sym_tbl e1 in
        let new_e2 = _type_check_expr component_ast_node component_sym function_sym_tbl e2 in
        let new_e3 = _type_check_expr component_ast_node component_sym function_sym_tbl e3 in
        begin
          match (new_e2.Ast.annot) with
          | Ast.TBool -> (Ast.For(Some new_e1, Some new_e2, Some new_e3, new_for_stmt)) @> (Ast.TVoid)
          | _ -> raise (Semantic_error(loc, "The for guard is not a boolean expression!"))
        end
      | (None, Some(e2), None) ->
        let new_e2 = _type_check_expr component_ast_node component_sym function_sym_tbl e2 in
        begin
          match (new_e2.Ast.annot) with
          | Ast.TBool -> (Ast.For(None, Some new_e2, None, new_for_stmt)) @> (Ast.TVoid)
          | _ -> raise (Semantic_error(loc, "The for guard is not a boolean expression!"))
        end
      | (None, None, None) -> (Ast.For(None, None, None, new_for_stmt)) @> (Ast.TVoid)
      | _ -> ignore ()
    end
  | Ast.Return(None) -> (Ast.Return(None)) @> Ast.TVoid
  | Ast.Return(Some(exp)) ->
      let new_exp = _type_check_expr component_ast_node component_sym function_sym_tbl exp in 
      (* Let's check if the expression type matchets with the function return type *)
      if Ast.equal_typ (new_exp.Ast.annot) (fun_rtype) then
        (Ast.Return(Some(new_exp))) @> (new_exp.Ast.annot)
      else
        raise (Semantic_error(loc, "The expression returned by the function does not match the function return type!"))
  | Ast.Block(stmts) ->
    (* create a new block in symbol table *)
    let _type_check_stmtordec annotated_node sym_tbl =
      let n = annotated_node.Ast.node in 
      let _l = annotated_node.Ast.annot in
      match n with
      | Ast.LocalDecl(vdecl) -> 
        let _ = _check_local_decl_type annotated_node in 
        (Ast.LocalDecl(vdecl)) @> Ast.TVoid
      | Ast.Stmt(stmt) ->
        let new_stmt = _type_check_stmt component_ast_node component_sym  sym_tbl stmt  in 
        (Ast.Stmt(new_stmt)) @> Ast.TVoid
    in
    let _fill_local_st sym_tbl annotated_node =
      let n = annotated_node.Ast.node in 
      let l = annotated_node.Ast.annot in
      match n with
      | Ast.LocalDecl(id, typ) -> 
        begin
          try
            Symbol_table.add_entry id (SVar({vattr = {id = id; loc = l; typ = typ}})) sym_tbl
          with Symbol_table.DuplicateEntry(_) ->
            let msg = Printf.sprintf "The variable %s has been declared twice!" id in 
            raise (Semantic_error(loc, msg))
        end
      | _ -> sym_tbl
    in
    let sym_table = (Symbol_table.begin_block function_sym_tbl) in
    let sym_table = List.fold_left _fill_local_st sym_table stmts in
    let new_stmts = List.map (fun x -> _type_check_stmtordec x (fun_rtype, sym_table)) stmts in
    (Ast.Block(new_stmts)) @> Ast.TVoid
  | Ast.Skip -> (Ast.Skip) @> Ast.TVoid

(** The function performs type checking *)
let second_pass ast global_table =

  let visit_component_member ast_node local_sym member_node =
    let node = member_node.Ast.node in 
    let _loc = member_node.Ast.annot in
    match node with
    | Ast.VarDecl(vd) -> (Ast.VarDecl(vd)) @> Ast.TVoid
    | Ast.FunDecl({Ast.body = None; _}) -> ignore ()
    | Ast.FunDecl({Ast.rtype; Ast.fname; Ast.formals; body = Some(fbody)}) ->
      let _ = _check_fun_rtype member_node in
      let csym_tbl = (match local_sym with SComponent({csym_tbl; _}) -> csym_tbl | _ -> ignore ()) in
      let fn_sym = Symbol_table.lookup fname csym_tbl in
      let fn_st = (match fn_sym with SFunction({fsym_tbl; _}) -> fsym_tbl | _ -> ignore ()) in
      let new_body = _type_check_stmt ast_node local_sym (rtype, fn_st) fbody in
      (* TODO: check retun statement inside function body *)
      (Ast.FunDecl({Ast.rtype = rtype; fname = fname; formals = formals; body = Some(new_body)})) @> Ast.TVoid
  in
  let visit_interface_member member_node =
    let node = member_node.Ast.node in 
    let _loc = member_node.Ast.annot in
    match node with
    | Ast.VarDecl(vd) -> (Ast.VarDecl(vd)) @> Ast.TVoid
    | Ast.FunDecl({Ast.rtype; Ast.fname; Ast.formals; body = None}) -> (Ast.FunDecl({Ast.rtype = rtype; fname = fname; formals = formals; body = None})) @> Ast.TVoid
    | Ast.FunDecl(_) -> ignore ()
  in
  let visit_component comp_node = 
    let node = comp_node.Ast.node in
    let _loc = comp_node.Ast.annot in
    match node with
    | Ast.ComponentDecl({cname = cname; uses = uses; provides = provides; definitions = definitions; }) ->
      let csym = Symbol_table.lookup cname global_table in
      let new_definitions = List.map (fun x -> visit_component_member node csym x) definitions in 
      let ctyp = Ast.TComponent(cname) in 
      (Ast.ComponentDecl({cname = cname; uses = uses; provides = provides; definitions = new_definitions})) @> ctyp
  in
  let visit_interface iface_node = 
    let node = iface_node.Ast.node in
    let _loc = iface_node.Ast.annot in
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
  let global_table = first_pass ast global_table in
  second_pass ast global_table