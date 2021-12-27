exception LinkingError of string

type symbol = 
  | SComponent of { cname: Ast.identifier; uses: Ast.identifier list; provides: Ast.identifier list; mutable links: symbol_table }
  | SLink of {use: Ast.identifier; provide: Ast.identifier} (* interface |-> component *)
and symbol_table = (symbol) Symbol_table.t

let ignore_pattern () = failwith "Should not happen"
let (@>) node annot = Ast.make_node node annot

let check_connections ast =
  let rec visit_components components component_sym_table = match components with
  | [] -> component_sym_table 
  | annotated_node::tail ->
    match annotated_node.Ast.node with
    | Ast.ComponentDecl({cname; uses; provides; _}) ->
      let csym = SComponent({cname = cname; uses = uses; provides = provides; links = Symbol_table.begin_block (Symbol_table.empty_table)}) in 
      visit_components tail (Symbol_table.add_entry cname csym component_sym_table)
  in
  let rec visit_connections connections component_sym_table = match connections with
  | [] -> component_sym_table
  | (Ast.Link(c1, _, c2, _))::_ when (c1 = "Prelude") || (c2 = "Prelude") ->
    raise (LinkingError("Link to Prelude interface cannot be specified!"))
  | (Ast.Link(c1, _, c2, _))::_ when (c1 = "App") || (c2 = "App") ->
    raise (LinkingError("Link to App interface cannot be specified!"))
  | (Ast.Link(c1, i1, c2, i2))::tail -> 
      (* Verify connection integrity: *)
      try
        (* Does the component c1 exists? And c2? *)
        let (c1_uses) = match Symbol_table.lookup c1 component_sym_table with SComponent({uses; _}) -> (uses) | _ -> ignore_pattern () in
        let (c2_provides) = match Symbol_table.lookup c2 component_sym_table with SComponent({provides; _}) -> (provides) | _ -> ignore_pattern () in
        (* Is i1 compatible with i2? *)
        (* Does the component c1 has i1 in the uses list? *)
        (* Does the component c2 has i2 in the provides list? *)
        match (i1 = i2, List.mem i1 c1_uses, List.mem i2 c2_provides) with
        | (false, _, _) ->
          let msg = Printf.sprintf "The link (%s.%s <- %s.%s) is not valid since the interface %s is not compatible with interface %s!" c1 i1 c2 i2 i1 i2 in
          raise (LinkingError(msg))
        | (_, false, _) ->
          let msg = Printf.sprintf "the interface `%s` is not inside the uses list of component `%s`!" i1 c1 in
          raise (LinkingError(msg))
        | (_, _, false) -> 
          let msg = Printf.sprintf "the interface `%s` is not inside the provides list of component `%s`!" i2 c2 in
          raise (LinkingError(msg))
        | (true, true, true) -> 
          begin
            (* Create a new SLink symbol and insert it inside the new symbol table *)
            try
              let lsym = SLink({use = i1; provide = c2}) in 
              match Symbol_table.lookup c1 component_sym_table with 
              | SComponent(csym) ->
                csym.links <- Symbol_table.add_entry i1 lsym csym.links;
                visit_connections tail component_sym_table
              | _ -> ignore_pattern()
            with Symbol_table.DuplicateEntry(_) ->
              let msg = Printf.sprintf "Double connection definition, the interface %s of component %s was already linked previously!" i1 c1 in
              raise (LinkingError(msg))
          end
      with Symbol_table.MissingEntry(id) ->
        let msg = Printf.sprintf "Undefined component %s in connect block!" id in
        raise (LinkingError(msg))
  in
  let rec check_components components sym_table = match components with
  | [] -> sym_table
  | annotated_node::tail ->
    let rec aux cname links_table = function 
    | [] -> ()
    | use::tail ->
      try
        let _ = Symbol_table.lookup use links_table in 
        aux cname links_table tail
      with Symbol_table.MissingEntry(_) ->
        let msg = Printf.sprintf "The interface %s was not linked in component %s!" use cname in 
        raise (LinkingError(msg))
    in
    let (cname, uses) = match annotated_node.Ast.node with Ast.ComponentDecl({cname; uses; _}) -> (cname, uses) in 
    let clinks = match Symbol_table.lookup cname sym_table with SComponent(sym) -> sym.links | _ -> ignore_pattern () in
    let _ = aux cname clinks uses in 
    check_components tail sym_table
  in
  let (components, connections) = (match ast with Ast.CompilationUnit({components; connections; _}) -> (components, connections)) in
  let component_sym_table = 
    (Symbol_table.begin_block (Symbol_table.empty_table))
    |> visit_components components 
    |> visit_connections connections
    |> check_components components
  in
  component_sym_table

let rec qualify_call_expression component_link_table node =
  match (node.Ast.node) with 
  | Ast.Call(Some iname, fun_name, exp_list) ->
    (* Qualify the interface name... *)
    begin
      try
        let cname = match Symbol_table.lookup iname component_link_table with SLink({provide; _}) -> provide | _ -> ignore_pattern () in
        Ast.Call(Some cname, fun_name, exp_list) @> (node.Ast.annot)
      with Symbol_table.MissingEntry(_) ->
        node
    end
  | Ast.Assign(lv, exp) ->
    let new_exp = qualify_call_expression component_link_table exp in
    (Ast.Assign(lv, new_exp)) @> (node.Ast.annot)
  | Ast.UnaryOp(uop, exp) -> 
    let new_exp = qualify_call_expression component_link_table exp in
    (Ast.UnaryOp(uop, new_exp)) @> (node.Ast.annot)
  | Ast.BinaryOp(bop, exp1, exp2) -> 
      let new_exp1 = qualify_call_expression component_link_table exp1 in
      let new_exp2 = qualify_call_expression component_link_table exp2 in
      (Ast.BinaryOp(bop, new_exp1, new_exp2)) @> (node.Ast.annot)
  | _ -> node
let rec visit_statements component_link_table node =
  match (node.Ast.node) with
  | Ast.If(exp, s1, s2) ->
    let new_exp1 = qualify_call_expression component_link_table exp in
    let new_s1 = visit_statements component_link_table s1 in
    let new_s2 = visit_statements component_link_table s2 in
    (Ast.If(new_exp1, new_s1, new_s2)) @> (node.Ast.annot)
  | Ast.While(exp, s1) ->
    let new_exp1 = qualify_call_expression component_link_table exp in
    let new_s1 = visit_statements component_link_table s1 in
    (Ast.While(new_exp1, new_s1)) @> (node.Ast.annot)
  | Ast.For(e1, e2, e3, s1) ->
    let new_exp1 = match e1 with None -> None | Some e -> Some (qualify_call_expression component_link_table e) in
    let new_exp2 = match e2 with None -> None | Some e -> Some (qualify_call_expression component_link_table e) in
    let new_exp3 = match e3 with None -> None | Some e -> Some (qualify_call_expression component_link_table e) in
    let new_s1 = visit_statements component_link_table s1 in
    Ast.For(new_exp1, new_exp2, new_exp3, new_s1) @> (node.Ast.annot)
  | Ast.Expr(exp) -> Ast.Expr(qualify_call_expression component_link_table exp) @> (node.Ast.annot)
  | Ast.Return(Some e) ->
    let new_exp = qualify_call_expression component_link_table e in 
    (Ast.Return(Some new_exp)) @> (node.Ast.annot)
  | Ast.Block(stmts) ->
    let new_stmts = visit_stmts_list component_link_table [] stmts in
    Ast.Block(new_stmts) @> (node.Ast.annot)
  | _ -> node
and visit_stmts_list component_link_table acc = function
  | [] -> acc
  | annotated_node::tail -> 
    match (annotated_node.Ast.node) with 
    | Ast.LocalDecl(_) -> visit_stmts_list component_link_table (annotated_node::acc) tail
    | Ast.Stmt(s) -> 
      let new_stmt = (Ast.Stmt(visit_statements component_link_table s)) @> (annotated_node.Ast.annot) in
      visit_stmts_list component_link_table (new_stmt::acc) tail
and visit_component_definitions component_link_table acc = function 
  | [] -> acc
  | annotated_node::tail ->
    match (annotated_node.Ast.node) with 
    | Ast.VarDecl(_) -> visit_component_definitions component_link_table (annotated_node::acc) tail
    | Ast.FunDecl({Ast.body = Some s; _} as fd) ->
      let new_stmt = visit_statements component_link_table s in
      let new_fun_decl = Ast.FunDecl({fd with Ast.body = Some new_stmt}) @> (annotated_node.Ast.annot) in 
      visit_component_definitions component_link_table (new_fun_decl::acc) tail
    | Ast.FunDecl({Ast.body = None; _}) -> ignore_pattern ()
and qualify_components acc global_table = function
  | [] -> acc 
  | annotated_node::tail ->
    match (annotated_node.Ast.node) with 
    | Ast.ComponentDecl(comp) ->
      let component_link_table = match Symbol_table.lookup comp.cname global_table with SComponent(csym) -> csym.links | _ -> ignore_pattern () in 
      let new_definitions = visit_component_definitions component_link_table [] comp.definitions in
      let new_component = Ast.ComponentDecl({comp with definitions = new_definitions}) @> annotated_node.Ast.annot in 
      qualify_components (new_component::acc) global_table tail

let link_phase ast global_table = 
  let (interfaces, components, connections) = match ast with Ast.CompilationUnit({interfaces; components; connections}) -> (interfaces, components, connections) in 
  let components = qualify_components [] global_table components in 
  Ast.CompilationUnit({interfaces = interfaces; components = components; connections = connections})

let wire_components ast =
  let global_table = check_connections ast in
  link_phase ast global_table