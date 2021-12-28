exception Codegen_error of string

type symbol_table = (Llvm.llvalue) Symbol_table.t
type fun_env = {mutable fsym_table: symbol_table; fun_def: Llvm.llvalue; current_module: Llvm.llmodule; ibuilder: Llvm.llbuilder}

let name_mangling cname name = 
  Printf.sprintf "__%s_%s" (String.lowercase_ascii cname) (String.lowercase_ascii name)

let ignore_pattern () = failwith "Should not happen!"
let not_implemented () = failwith "Not implemented, yet"

(* Get the reference from the global LLVM context *)
let global_context = Llvm.global_context ()
let _ = Llvm.enable_pretty_stacktrace ()

(* Get basics type from LLVM global context *)
let void_type = Llvm.void_type global_context
and i1_type = Llvm.i1_type global_context
and i8_type = Llvm.i8_type global_context
and i32_type = Llvm.i32_type global_context

let rec mucomp_type_to_llvm typ =
  match typ with 
  | Ast.TBool -> i1_type
  | Ast.TInt -> i32_type
  | Ast.TChar -> i8_type
  | Ast.TArray(t, Some n) -> Llvm.array_type (mucomp_type_to_llvm t) n
  | Ast.TArray(t, None)
  | Ast.TRef(t) -> Llvm.pointer_type (mucomp_type_to_llvm t)
  | Ast.TVoid -> void_type
  | Ast.TFun(formals, rtyp) ->
    let f_rtype = mucomp_type_to_llvm rtyp in
    let formals = List.map (mucomp_type_to_llvm) formals in 
    let f_formals = Array.init (List.length formals) (fun i -> List.nth formals i) in 
    Llvm.function_type f_rtype f_formals
  | _ -> ignore_pattern ()

let get_default_value typ =
  match typ with
  | Ast.TInt -> Llvm.const_int i32_type 0 (* default value for numbers is zero *)
  | Ast.TBool -> Llvm.const_int i1_type 0 (* default values for booleans is false *)
  | Ast.TChar -> Llvm.const_int i8_type 0 (* default values for chars is terminator *)
  | _ -> not_implemented ()

let rec _eval_exp node fun_env = 
  match node.Ast.node with
  | Ast.LV(lv) -> 
    let temp_lv = _eval_lv lv fun_env in 
    Llvm.build_load (snd temp_lv) ("r_" ^ (fst temp_lv)) fun_env.ibuilder
  | Ast.Assign(lv, exp) ->
    let new_exp = _eval_exp exp fun_env in
    let var = _eval_lv lv fun_env in
    let _ = Llvm.build_store new_exp (snd var) fun_env.ibuilder in
    new_exp
  | Ast.ILiteral(i) -> Llvm.const_int i32_type i
  | Ast.CLiteral(c) -> Llvm.const_int i8_type (int_of_char c)
  | Ast.BLiteral(b) -> Llvm.const_int i1_type (if b then 1 else 0)
  | Ast.BinaryOp(bop, e1, e2) ->
    let new_e1 = _eval_exp e1 fun_env in
    let new_e2 = _eval_exp e2 fun_env in
    begin
      match (bop) with 
      | Ast.Add -> Llvm.build_add new_e1 new_e2 "temp_add" fun_env.ibuilder
      | Ast.Sub -> Llvm.build_sub new_e1 new_e2 "temp_sub" fun_env.ibuilder
      | Ast.Mult -> Llvm.build_mul new_e1 new_e2 "temp_mult" fun_env.ibuilder
      | Ast.Div -> Llvm.build_udiv new_e1 new_e2 "temp_div" fun_env.ibuilder
      | Ast.Mod -> Llvm.build_urem new_e1 new_e2 "temp_rem" fun_env.ibuilder
      | Ast.Equal -> Llvm.build_icmp (Llvm.Icmp.Eq) new_e1 new_e2 "temp_eq" fun_env.ibuilder
      | Ast.Neq -> Llvm.build_icmp (Llvm.Icmp.Ne) new_e1 new_e2 "temp_neq" fun_env.ibuilder
      | Ast.Less -> Llvm.build_icmp (Llvm.Icmp.Ult) new_e1 new_e2 "temp_less" fun_env.ibuilder
      | Ast.Leq -> Llvm.build_icmp (Llvm.Icmp.Ule) new_e1 new_e2 "temp_leq" fun_env.ibuilder
      | Ast.Greater -> Llvm.build_icmp (Llvm.Icmp.Ugt) new_e1 new_e2 "temp_greater" fun_env.ibuilder
      | Ast.Geq -> Llvm.build_icmp (Llvm.Icmp.Uge) new_e1 new_e2 "temp_geq" fun_env.ibuilder
      | Ast.And -> Llvm.build_and new_e1 new_e2 "temp_and" fun_env.ibuilder
      | Ast.Or -> Llvm.build_or new_e1 new_e2 "temp_or" fun_env.ibuilder
    end
  | Ast.UnaryOp(uop, exp) ->
    let new_exp = _eval_exp exp fun_env in
    begin
      match uop with 
      | Ast.Neg -> Llvm.build_neg new_exp "temp_neg" fun_env.ibuilder 
      | Ast.Not -> Llvm.build_not new_exp "temp_not" fun_env.ibuilder
    end
  | Ast.Address(lv) ->
    let var = _eval_lv lv fun_env in 
    Llvm.build_gep (snd var) [|(Llvm.const_int i32_type 0);|] "temp_gep_address" fun_env.ibuilder
  | Ast.Call(Some cname, fname, exp_list) ->
    let new_exp_array = Array.of_list (List.map (fun e -> _eval_exp e fun_env) exp_list) in
    let fun_name = name_mangling cname fname in
    let func = Llvm.lookup_function fun_name fun_env.current_module in
    begin
      match (func, node.Ast.annot) with
      | (None, _) -> raise (Codegen_error((Printf.sprintf "Cannot find `%s` global function!" fun_name)))
      | (Some func, Ast.TVoid) ->
        Llvm.build_call func new_exp_array "" fun_env.ibuilder (* if the function return a void then don't assign a return value *)
      | (Some func, _) ->
        Llvm.build_call func new_exp_array ("call_" ^ fun_name) fun_env.ibuilder
    end
  | _ -> ignore_pattern ()
and _eval_lv node fun_env = 
    match node.Ast.node with
    | Ast.AccVar(None, vid) ->
      (* Search in function symbol table the identifier *)
      begin
        try
          (vid, Symbol_table.lookup vid fun_env.fsym_table)
        with Symbol_table.MissingEntry(_) ->
          let msg = "There is something wrong here..." in
          raise (Codegen_error(msg))
      end
    | Ast.AccVar(Some cname, vid) ->
      (* Search inside global scope *)
      let nm = name_mangling cname vid in
      begin
        match (Llvm.lookup_global nm fun_env.current_module) with
        | None -> raise (Codegen_error("Cannot found global variable declaration!")) (* TODO: handle this case? *)
        | Some v -> (nm, v)
      end
    | Ast.AccIndex(lv, exp) ->
      (* We are accessing an array element *)
      let array = _eval_lv lv fun_env in
      let index = _eval_exp exp fun_env in 
      ((fst array), Llvm.build_in_bounds_gep (snd array) [|(Llvm.const_int i32_type 0); index|] "temp_gep_arr_el" fun_env.ibuilder)
and _eval_stmt node fun_env =
  match node.Ast.node with
  | Ast.Expr(e) ->
      let _ = _eval_exp e fun_env in ()
  | Ast.Skip -> ()
  | Ast.If(exp, s1, s2) ->
      let thenbb = Llvm.append_block global_context "then" fun_env.fun_def in
      let elsebb = Llvm.append_block global_context "else" fun_env.fun_def in
      let mergebb = Llvm.append_block global_context "merge" fun_env.fun_def in
      let guard_exp = _eval_exp exp fun_env in
      let _ = Llvm.build_cond_br guard_exp thenbb elsebb fun_env.ibuilder in 
      Llvm.position_at_end thenbb fun_env.ibuilder;
      _eval_stmt s1 fun_env;
      let _ = Llvm.build_br mergebb fun_env.ibuilder in
      Llvm.position_at_end elsebb fun_env.ibuilder;
      _eval_stmt s2 fun_env;
      let _ = Llvm.build_br mergebb fun_env.ibuilder in
      Llvm.position_at_end mergebb fun_env.ibuilder;
  | Ast.While(exp, s1) ->
    let condbb = Llvm.append_block global_context "cond" fun_env.fun_def in
    let loopbb = Llvm.append_block global_context "loop" fun_env.fun_def in 
    let after_loop = Llvm.append_block global_context "after_loop" fun_env.fun_def in
    let _ = Llvm.build_br condbb fun_env.ibuilder in 
    Llvm.position_at_end condbb fun_env.ibuilder;
    let guard_exp = _eval_exp exp fun_env in
    let _ = Llvm.build_cond_br guard_exp loopbb after_loop fun_env.ibuilder in 
    Llvm.position_at_end loopbb fun_env.ibuilder;
    let _ = _eval_stmt s1 fun_env in 
    let _ = Llvm.build_br condbb fun_env.ibuilder in 
    Llvm.position_at_end after_loop fun_env.ibuilder;
  | Ast.Block(b) ->
    (* We are entering in a new inner block. *)
    (* Push a new block inside the symbol table. *)
    fun_env.fsym_table <- Symbol_table.begin_block fun_env.fsym_table;
    (* Emit block instructions *)
    List.iter (fun s -> _eval_stmtordec s fun_env) b;
    (* Pop the block from the symbol table since we are exiting from the block *)
    fun_env.fsym_table <- Symbol_table.end_block fun_env.fsym_table;
  | Ast.Return(Some e) -> 
    let _ = Llvm.build_ret (_eval_exp e fun_env) fun_env.ibuilder in ()
  | Ast.Return(None) -> 
    let _ = Llvm.build_ret_void fun_env.ibuilder in ()
  | _ -> ignore_pattern ()
and _eval_stmtordec node fun_env = 
  match node.Ast.node with
  | Ast.LocalDecl(vid, vtyp) -> 
    (* Put llvalue inside the current block *)
    let llvalue = Llvm.build_alloca (mucomp_type_to_llvm vtyp) vid fun_env.ibuilder in 
    (* Update symbol table *)
    fun_env.fsym_table <- Symbol_table.add_entry vid llvalue fun_env.fsym_table;
    ()
  | Ast.Stmt(s) ->
    let _ = _eval_stmt s fun_env in ()
and _eval_member_decl node cname is_main_component current_module decl_st =
  match node.Ast.node with 
  | Ast.FunDecl({Ast.fname; Ast.formals; Ast.body = Some body; _}) ->
    (* Remove the old function declaration... *)
    let fun_name = if (is_main_component && fname = "main") then "main" else name_mangling cname fname in
    let _ = Llvm.delete_function (Symbol_table.lookup fun_name decl_st) in
    (* Define a new function environment *)
    let fun_st = Symbol_table.begin_block (Symbol_table.empty_table) in
    (* Define a new function from the current component. The function type is held by the node annotation. *)
    let fun_type = mucomp_type_to_llvm (node.Ast.annot) in
    let fun_def = Llvm.define_function fun_name fun_type current_module in
    (* Build a new instruction builder to generate instruction for the function *)
    let fun_ibuilder = Llvm.builder_at_end global_context (Llvm.entry_block fun_def) in
    (* Allocate and store function paramaters inside the stack. *)
    let fun_st = List.fold_left (fun acc (idx, (vid, vtyp)) -> 
      let param_stack = Llvm.build_alloca (mucomp_type_to_llvm vtyp) vid fun_ibuilder in
      let param = Llvm.param fun_def idx in
      let _ = Llvm.build_store param param_stack fun_ibuilder in
      Symbol_table.add_entry vid param_stack acc
    ) fun_st (Mcomp_stdlib.list_zip_with_index formals) in
    (* Evaluate statement. *)
    let fun_env = {fsym_table = fun_st; ibuilder = fun_ibuilder; fun_def = fun_def; current_module = current_module} in
    let _ = _eval_stmt body fun_env in ()
  | Ast.VarDecl(vid, vtyp) -> 
    (* Remove the old variable declaration... *)
    let var_name = name_mangling cname vid in 
    let _ = Llvm.delete_function (Symbol_table.lookup var_name decl_st) in
    (* We initialize the variable with value equal to zero if possible *)
    let initialized_value = get_default_value vtyp in 
    let _ = Llvm.define_global var_name initialized_value current_module in ()
  | _ -> ignore_pattern ()
and _declare_component_members node cname is_main_component current_module acc = 
  match node.Ast.node with
  | Ast.FunDecl({Ast.fname; _}) ->
    (* Declare the function and put it inside the symbol table *)
    let fun_type = mucomp_type_to_llvm (node.Ast.annot) in
    let fun_name = if (is_main_component && fname = "main") then "main" else name_mangling cname fname in
    let fun_decl = Llvm.declare_function fun_name fun_type current_module in
    Symbol_table.add_entry fun_name fun_decl acc
  | Ast.VarDecl(vid, vtyp) -> 
    let var_type = mucomp_type_to_llvm vtyp in 
    let var_name = name_mangling cname vid in 
    let var_decl = Llvm.declare_global var_type var_name current_module in
    Symbol_table.add_entry var_name var_decl acc
and _eval_component_decl node temp_st current_module =
  match node.Ast.node with
  | Ast.ComponentDecl({cname; provides; definitions; _}) ->
    let is_main_component = (List.mem "App" provides) in
    let _ = List.iter (fun m -> _eval_member_decl m cname is_main_component current_module temp_st) definitions in ()
and _declare_component node current_module acc =
  match node.Ast.node with
  | Ast.ComponentDecl({cname; provides; definitions; _}) ->
    let is_main_component = (List.mem "App" provides) in
    (* First declare the component members ... *)
    List.fold_left (fun acc m -> _declare_component_members m cname is_main_component current_module acc) acc definitions

let to_llvm_module ast = 
  (* The module disposition is handled somewhere else... *)
  let global_module = Llvm.create_module global_context "global-module" in
  (* Declare prelude functions *)
  let _ = List.iter (fun (id, typ) -> let _ = Llvm.declare_function (name_mangling "Prelude" id) (mucomp_type_to_llvm typ) global_module in ()) Mcomp_stdlib.prelude_signature in
  (* Declare components *)
  let components = match ast with Ast.CompilationUnit(cu) -> cu.components in 
  let temp_st = List.fold_left (fun acc c -> _declare_component c global_module acc) (Symbol_table.begin_block (Symbol_table.empty_table)) components in
  ignore (Printf.printf "%s\n" (Symbol_table.show temp_st));
  (* Define components *)
  let _ = List.iter (fun c -> _eval_component_decl c temp_st global_module) components in
  global_module
