exception Codegen_error of string

type symbol_table = (Llvm.llvalue) Symbol_table.t
type fun_env = {
  mutable fsym_table: symbol_table;             (* Function symbol table which holds Llvm.llvalue for local variables *)
  fun_def: Llvm.llvalue;                        (* The function definition. *)
  current_module: Llvm.llmodule;                (* The current module where the function is defined. *)
  ibuilder: Llvm.llbuilder;                     (* The function instruction builder to emit code. *)
}

type global_env = {
  global_module: Llvm.llmodule;
  global_ctors: Llvm.llvalue * Llvm.llbuilder;
  global_var_init: Llvm.llvalue * Llvm.llbuilder;
}

type comp_env = {
  cname: string;
  is_main: bool;
  current_module: Llvm.llmodule;
  global_env: global_env;
}

(* Do name mangling using the component and member names. *)
let name_mangling cname name = 
  Printf.sprintf "__%s_%s" (String.lowercase_ascii cname) (String.lowercase_ascii name)

let ignore_pattern () = failwith "Should not happen!"
let _not_implemented () = failwith "Not implemented, yet"

(* Get the reference from the global LLVM context *)
let global_context = Llvm.global_context ()

(* Get basics type from LLVM global context *)
let void_type = Llvm.void_type global_context
and i1_type = Llvm.i1_type global_context
and i8_type = Llvm.i8_type global_context
and i32_type = Llvm.i32_type global_context
and float_type = Llvm.float_type global_context

(* Convert a mucomp_lang type into a LLVM one. *)
let rec mucomp_type_to_llvm typ =
  match typ with 
  | Ast.TBool -> i1_type
  | Ast.TInt -> i32_type
  | Ast.TChar -> i8_type
  | Ast.TFloat -> float_type
  | Ast.TArray(t, Some n) -> Llvm.array_type (mucomp_type_to_llvm t) n
  | Ast.TArray(t, None) (* We treat an empty array as a pointer. *)
  | Ast.TRef(t) -> Llvm.pointer_type (mucomp_type_to_llvm t)
  | Ast.TVoid -> void_type
  | Ast.TFun(formals, rtyp) ->
    let f_rtype = mucomp_type_to_llvm rtyp in
    let formals = List.map (mucomp_type_to_llvm) formals in 
    let f_formals = Array.init (List.length formals) (fun i -> List.nth formals i) in 
    Llvm.function_type f_rtype f_formals
  | _ -> ignore_pattern () (* Components and interfaces don't have a corresponding LLVM type. *)

(* Return an llvalue used to initialize a variable. *)
let get_default_value typ =
  match typ with
  | Ast.TInt -> Llvm.const_int i32_type 0 (* default value for numbers is zero *)
  | Ast.TBool -> Llvm.const_int i1_type 0 (* default values for booleans is false *)
  | Ast.TChar -> Llvm.const_int i8_type 0 (* default values for chars is terminator *)
  | Ast.TFloat -> Llvm.const_float float_type 0.0
  | Ast.TArray(styp, Some n) ->           (* initialize an array containing only zeros *)
    let llscalar_type = mucomp_type_to_llvm styp in
    let initial_array_values = Array.init n (fun _ -> (Llvm.const_int llscalar_type 0)) in
    Llvm.const_array llscalar_type initial_array_values
  | _ -> ignore_pattern ()

(* Auxiliar function to build an alloca instruction depending on the value type. *)
let aux_build_alloca id typ builder =
  let lltype = mucomp_type_to_llvm typ in 
  match typ with
  | Ast.TArray(_, Some n) -> Llvm.build_array_alloca lltype (Llvm.const_int i32_type n) id builder
  | _ -> Llvm.build_alloca lltype id builder

let unify_blocks f_rtype fun_env =
  let count_aux acc bb =
    match Llvm.block_terminator bb with
    | None -> acc 
    | Some it -> begin
      match Llvm.instr_opcode it with
      | Llvm.Opcode.Ret -> bb :: acc 
      | _ -> acc
    end
  in
  (* Count how many return instructions are inside the function. *)
  let block_containing_returns = Llvm.fold_left_blocks count_aux [] fun_env.fun_def |> List.rev in 
  match (List.length block_containing_returns) with
  | 0
  | 1 -> (* Leave it as it is... *) ()
  | _ -> (* We do have more than 1 block with returns *)
    begin
      match f_rtype with
      | Ast.TVoid -> ()
      | _ ->
        (* Create a new basic block containing the phi node to return the value *)
        let retbb = Llvm.append_block global_context "ret.merge" fun_env.fun_def in
        Llvm.position_at_end retbb fun_env.ibuilder;
        let phi_node = Llvm.build_empty_phi (mucomp_type_to_llvm f_rtype) "" fun_env.ibuilder in
        ignore(Llvm.build_ret phi_node fun_env.ibuilder);
        List.iter (fun bb -> 
          (* 1) Remove terminator instruction and get it's expression value *)
          let terminator = Option.get (Llvm.block_terminator bb) in
          let ret_value = Llvm.operand terminator 0 in
          Llvm.delete_instruction terminator;
          (* 2) Branch to return block. *)
          Llvm.position_at_end bb fun_env.ibuilder;
          ignore(Llvm.build_br retbb fun_env.ibuilder);
          (* 3) Add block to phi node. *) 
          Llvm.add_incoming (ret_value, bb) phi_node;
        ) block_containing_returns;
    end

let remove_useless_returns fun_env = 
  (* After the firsr return in each block remove successors instructions *)
  ignore(Llvm.iter_blocks (fun bb -> 
    let counted_return = ref 0 in 
    let to_delete = Llvm.fold_left_instrs (fun acc ins -> 
      if !counted_return > 0 then
        ins::acc
      else
        match (Llvm.is_terminator ins, Llvm.instr_opcode ins) with
        | (true, Llvm.Opcode.Ret) ->
          counted_return := !counted_return + 1;
          acc
        | (_, _) -> acc
    ) [] bb in 
    List.iter Llvm.delete_instruction to_delete
  ) fun_env.fun_def)

let fill_missing_returns rtype fun_env = 
  match rtype with 
  | Ast.TVoid ->
    Llvm.iter_blocks (fun bb -> 
      match Llvm.block_terminator bb with
      | Some _ -> ()
      | None -> 
        (* A terminator is missing! *)
        Llvm.position_at_end bb fun_env.ibuilder;
        ignore(Llvm.build_ret_void fun_env.ibuilder);  
    ) fun_env.fun_def;
  | _ -> ()

let remove_empty_blocks fun_env =
  let to_delete = Llvm.fold_left_blocks (fun acc bb -> 
    match Llvm.block_terminator bb with
    | Some _ -> acc
    | None -> bb :: acc  
  ) [] fun_env.fun_def in
  List.iter (fun bb -> Llvm.delete_block bb) to_delete

let rec eval_lv ?(just_address=false) node fun_env load =

  let aux_load lv_llvalue typ = 
    match typ with
    | Ast.TRef(_) ->
      if just_address then 
        lv_llvalue
      else
        let l1 = Llvm.build_load lv_llvalue "" fun_env.ibuilder in
        if load then Llvm.build_load l1 "" fun_env.ibuilder else l1
    | Ast.TArray(_, Some _) ->
      Llvm.build_in_bounds_gep lv_llvalue [|(Llvm.const_int i32_type 0); (Llvm.const_int i32_type 0)|] "" fun_env.ibuilder
    | _ ->
      if load then Llvm.build_load lv_llvalue "" fun_env.ibuilder else lv_llvalue
  in

  match (node.Ast.node, node.Ast.annot) with
  | (Ast.AccVar(None, vid), typ) -> 
    let lv_llvalue = Symbol_table.lookup vid fun_env.fsym_table in 
    aux_load lv_llvalue typ    

  | (Ast.AccVar(Some cname, vid), typ) -> 
    let mangled_name = name_mangling cname vid in
    let global_var = Llvm.lookup_global mangled_name fun_env.current_module in
    begin
      match global_var with
      | None -> raise (Codegen_error("Global variable not found!"))
      | Some lv_llvalue -> 
        aux_load lv_llvalue typ
    end

  | (Ast.AccIndex(lv, exp), _) ->
    let aux' lv idx = 
      let lv_llvalue = match lv.Ast.node with
      | Ast.AccVar(Some cname, vid) -> 
        begin 
          match Llvm.lookup_global (name_mangling cname vid) fun_env.current_module with 
          | None -> raise (Codegen_error("Global variable not found"))
          | Some var -> var
        end
      | Ast.AccVar(None, vid) -> Symbol_table.lookup vid fun_env.fsym_table 
      | _ -> ignore_pattern () 
      in
      begin
        match (lv.Ast.annot) with
        | Ast.TArray(Ast.TRef(_), Some _) ->
          let array_element = Llvm.build_in_bounds_gep lv_llvalue [|(Llvm.const_int i32_type 0); idx|] "" fun_env.ibuilder in
          if load then
            let loaded = Llvm.build_load array_element "" fun_env.ibuilder in
            Llvm.build_load loaded "" fun_env.ibuilder
          else
            array_element
        | Ast.TArray(_, Some _) ->
          let array_element = Llvm.build_in_bounds_gep lv_llvalue [|(Llvm.const_int i32_type 0); idx|] "" fun_env.ibuilder in
          if load then
            Llvm.build_load array_element "" fun_env.ibuilder
          else
            array_element
        | Ast.TArray(_, None)
        | Ast.TRef(_) ->
          let loaded_address = Llvm.build_load lv_llvalue "" fun_env.ibuilder in
          let array_element = Llvm.build_in_bounds_gep loaded_address [|idx|] "" fun_env.ibuilder in
          if load then
            Llvm.build_load array_element "" fun_env.ibuilder
          else
            array_element
        | _ -> ignore_pattern ()
      end
    in
    let llv_exp = eval_exp exp fun_env in aux' lv llv_exp
    
and eval_bool_and_exp lhs rhs fun_env =

  let llvalue_exp1 = eval_exp lhs fun_env in
  let coming_bb1 = Llvm.insertion_block fun_env.ibuilder in

  let true_bb = Llvm.append_block global_context "and.true" fun_env.fun_def in
  let false_bb = Llvm.append_block global_context "and.false" fun_env.fun_def in

  ignore(Llvm.build_cond_br llvalue_exp1 true_bb false_bb fun_env.ibuilder);
  ignore(Llvm.position_at_end true_bb fun_env.ibuilder);

  let llvalue_exp2 = eval_exp rhs fun_env in 
  let coming_bb2 = Llvm.insertion_block fun_env.ibuilder in

  ignore(Llvm.build_br false_bb fun_env.ibuilder);
  ignore(Llvm.position_at_end false_bb fun_env.ibuilder);

  let phi_node = Llvm.build_empty_phi i1_type "" fun_env.ibuilder in 
  let incomings = [(llvalue_exp1, coming_bb1); (llvalue_exp2, coming_bb2)] in 
  ignore(List.iter (fun p -> Llvm.add_incoming p phi_node) incomings);

  phi_node

and eval_bool_or_exp lhs rhs fun_env =

  let llvalue_exp1 = eval_exp lhs fun_env in
  let coming_bb1 = Llvm.insertion_block fun_env.ibuilder in

  let false_bb = Llvm.append_block global_context "or.false" fun_env.fun_def in
  let true_bb = Llvm.append_block global_context "or.true" fun_env.fun_def in

  ignore(Llvm.build_cond_br llvalue_exp1 true_bb false_bb fun_env.ibuilder);
  ignore(Llvm.position_at_end false_bb fun_env.ibuilder);

  let llvalue_exp2 = eval_exp rhs fun_env in 
  let coming_bb2 = Llvm.insertion_block fun_env.ibuilder in

  ignore(Llvm.build_br true_bb fun_env.ibuilder);
  ignore(Llvm.position_at_end true_bb fun_env.ibuilder);

  let phi_node = Llvm.build_empty_phi i1_type "" fun_env.ibuilder in 
  let incomings = [(llvalue_exp1, coming_bb1); (llvalue_exp2, coming_bb2)] in 
  ignore(List.iter (fun p -> Llvm.add_incoming p phi_node) incomings);

  phi_node

and eval_exp node fun_env = 
  match node.Ast.node with
  | Ast.LV(lv) -> 
    eval_lv lv fun_env true 

  | Ast.Assign(lv, exp) ->

    let j_address = match (lv.Ast.annot, exp.Ast.node) with 
      | (Ast.TRef(_), Ast.Address(_)) -> true 
      | _ -> false in

    let exp_llvalue = eval_exp exp fun_env in
    let lv_llvalue = eval_lv ~just_address:j_address lv fun_env false in
    (* Let's evaluate the expression to get an llvalue *)
    ignore (Llvm.build_store exp_llvalue lv_llvalue fun_env.ibuilder);
    exp_llvalue

  | Ast.ILiteral(i) -> Llvm.const_int i32_type i
  | Ast.CLiteral(c) -> Llvm.const_int i8_type (int_of_char c)
  | Ast.BLiteral(b) -> Llvm.const_int i1_type (if b then 1 else 0)
  | Ast.FLiteral(f) -> Llvm.const_float float_type f

  | Ast.BinaryOp(bop, e1, e2) ->
    
    let common_check = function
    | (Ast.TRef(t1), t2) when Ast.equal_typ t1 t2 -> true
    | (t1, Ast.TRef(t2)) when Ast.equal_typ t1 t2 -> true
    | (t1, t2) when Ast.equal_typ t1 t2 -> true
    | _ -> false
    in

    begin
      match bop with
      | Ast.And -> eval_bool_and_exp e1 e2 fun_env
      | Ast.Or ->  eval_bool_or_exp e1 e2 fun_env
      | _ ->

        let new_e1 = eval_exp e1 fun_env in
        let new_e2 = eval_exp e2 fun_env in
        match (bop, e1.Ast.annot, e2.Ast.annot) with 
        | (Ast.Add, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))      -> Llvm.build_add new_e1 new_e2 "temp.add" fun_env.ibuilder
        | (Ast.Add, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))    -> Llvm.build_fadd new_e1 new_e2 "temp.fadd" fun_env.ibuilder
        
        | (Ast.Sub, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))      -> Llvm.build_sub new_e1 new_e2 "temp.sub" fun_env.ibuilder
        | (Ast.Sub, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))    -> Llvm.build_fsub new_e1 new_e2 "temp.fsub" fun_env.ibuilder
        
        | (Ast.Mult, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))     -> Llvm.build_mul new_e1 new_e2 "temp.mult" fun_env.ibuilder
        | (Ast.Mult, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))   -> Llvm.build_fmul new_e1 new_e2 "temp.fmult" fun_env.ibuilder

        | (Ast.Div, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))      -> Llvm.build_udiv new_e1 new_e2 "temp.div" fun_env.ibuilder
        | (Ast.Div, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))    -> Llvm.build_fdiv new_e1 new_e2 "temp.fdiv" fun_env.ibuilder

        | (Ast.Mod, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))      -> Llvm.build_srem new_e1 new_e2 "temp.rem" fun_env.ibuilder
        | (Ast.Mod, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))    -> Llvm.build_frem new_e1 new_e2 "temp.frem" fun_env.ibuilder

        | (Ast.Equal, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))      -> Llvm.build_icmp (Llvm.Icmp.Eq) new_e1 new_e2 "temp.eq" fun_env.ibuilder
        | (Ast.Equal, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))    -> Llvm.build_fcmp (Llvm.Fcmp.Oeq) new_e1 new_e2 "temp.feq" fun_env.ibuilder
        
        | (Ast.Neq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))        -> Llvm.build_icmp (Llvm.Icmp.Ne) new_e1 new_e2 "temp.neq" fun_env.ibuilder
        | (Ast.Neq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TBool) && (common_check (t1, t2))       -> Llvm.build_icmp (Llvm.Icmp.Ne) new_e1 new_e2 "temp.neq" fun_env.ibuilder
        | (Ast.Neq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TChar) && (common_check (t1, t2))       -> Llvm.build_icmp (Llvm.Icmp.Ne) new_e1 new_e2 "temp.neq" fun_env.ibuilder
        | (Ast.Neq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))      -> Llvm.build_fcmp (Llvm.Fcmp.One) new_e1 new_e2 "temp.fneq" fun_env.ibuilder

        | (Ast.Less, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))       -> Llvm.build_icmp (Llvm.Icmp.Slt) new_e1 new_e2 "temp.less" fun_env.ibuilder
        | (Ast.Less, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))     -> Llvm.build_fcmp (Llvm.Fcmp.Oeq) new_e1 new_e2 "temp.fless" fun_env.ibuilder
        
        | (Ast.Leq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))        -> Llvm.build_icmp (Llvm.Icmp.Sle) new_e1 new_e2 "temp.leq" fun_env.ibuilder
        | (Ast.Leq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))        -> Llvm.build_fcmp (Llvm.Fcmp.Ole) new_e1 new_e2 "temp.fleq" fun_env.ibuilder
        
        | (Ast.Greater, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))    -> Llvm.build_icmp (Llvm.Icmp.Sgt) new_e1 new_e2 "temp.greater" fun_env.ibuilder
        | (Ast.Greater, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))  -> Llvm.build_fcmp (Llvm.Fcmp.Ogt) new_e1 new_e2 "temp.fgreater" fun_env.ibuilder
        
        | (Ast.Geq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TInt) && (common_check (t1, t2))         -> Llvm.build_icmp (Llvm.Icmp.Sge) new_e1 new_e2 "temp.geq" fun_env.ibuilder
        | (Ast.Geq, t1, t2) when (Ast.equal_typ (Ast.base_typ t1) Ast.TFloat) && (common_check (t1, t2))       -> Llvm.build_fcmp (Llvm.Fcmp.Oge) new_e1 new_e2 "temp.fgeq" fun_env.ibuilder
        
        | _ -> ignore_pattern ()
    end

  | Ast.UnaryOp(uop, exp) ->
    let new_exp = eval_exp exp fun_env in
    begin
      match uop with 
      | Ast.Neg -> Llvm.build_neg new_exp "temp.neg" fun_env.ibuilder 
      | Ast.Not -> Llvm.build_not new_exp "temp.not" fun_env.ibuilder
    end

  | Ast.Address(lv) -> 
    let ll_lv = eval_lv ~just_address:true lv fun_env false in
    ll_lv

  | Ast.Call(Some cname, fname, exp_list) ->
    let new_exp_array = Array.of_list (List.map (fun e -> eval_exp e fun_env) exp_list) in
    let fun_name = name_mangling cname fname in
    let func = Llvm.lookup_function fun_name fun_env.current_module in
    begin
      match (func, node.Ast.annot) with
      | (None, _) -> raise (Codegen_error((Printf.sprintf "Cannot find `%s` global function!" fun_name)))
      | (Some func, Ast.TVoid) ->
        Llvm.build_call func new_exp_array "" fun_env.ibuilder (* if the function return a void then don't assign a return value *)
      | (Some func, _) ->
        Llvm.build_call func new_exp_array ("call." ^ fun_name) fun_env.ibuilder
    end

  | Ast.DoubleOp(dop, dop_prec, lv) ->
    begin
      let lv_llvalue = eval_lv lv fun_env true in
      let (build_add, build_sub, one_ll) = (match lv.Ast.annot with 
        | Ast.TInt -> (Llvm.build_add, Llvm.build_sub, Llvm.const_int i32_type 1)
        | Ast.TFloat -> (Llvm.build_fadd, Llvm.build_fsub, Llvm.const_float float_type 1.0)
        | _ -> ignore_pattern ()
      )
      in
      match (dop, dop_prec) with
      | (Ast.PlusPlus, Ast.Post) ->
        let plus_one = build_add lv_llvalue one_ll "" fun_env.ibuilder in 
        let _ = Llvm.build_store plus_one (eval_lv lv fun_env false) fun_env.ibuilder in
        lv_llvalue
      | (Ast.PlusPlus, Ast.Pre) -> 
        let plus_one = build_add lv_llvalue one_ll "" fun_env.ibuilder in 
        let _ = Llvm.build_store plus_one (eval_lv lv fun_env false) fun_env.ibuilder in
        plus_one
      | (Ast.MinMin, Ast.Post) ->
        let plus_one = build_sub lv_llvalue one_ll "" fun_env.ibuilder in 
        let _ = Llvm.build_store plus_one (eval_lv lv fun_env false) fun_env.ibuilder in
        lv_llvalue
      | (Ast.MinMin, Ast.Pre) -> 
        let plus_one = build_sub lv_llvalue one_ll "" fun_env.ibuilder in 
        let _ = Llvm.build_store plus_one (eval_lv lv fun_env false) fun_env.ibuilder in
        plus_one
    end

  | _ -> ignore_pattern ()    

and eval_stmt node fun_env =
  match node.Ast.node with
  | Ast.Expr(e) -> ignore(eval_exp e fun_env); false

  | Ast.Skip -> false

  | Ast.If(exp, s1, s2) ->
      let guard_exp = eval_exp exp fun_env in
      let thenbb = Llvm.append_block global_context "if.then" fun_env.fun_def in
      let elsebb = Llvm.append_block global_context "if.else" fun_env.fun_def in
      let mergebb = Llvm.append_block global_context "if.merge" fun_env.fun_def in
      (* Build conditional branch *)
      ignore(Llvm.build_cond_br guard_exp thenbb elsebb fun_env.ibuilder);
      Llvm.position_at_end thenbb fun_env.ibuilder;
      let s1_has_return = eval_stmt s1 fun_env in
      if s1_has_return then () else ignore(Llvm.build_br mergebb fun_env.ibuilder);
      Llvm.position_at_end elsebb fun_env.ibuilder;
      let s2_has_return = eval_stmt s2 fun_env in
      if s2_has_return then () else ignore(Llvm.build_br mergebb fun_env.ibuilder) ;
      Llvm.position_at_end mergebb fun_env.ibuilder;
      s1_has_return && s2_has_return

  | Ast.While(exp, s1) ->
    let condbb = Llvm.append_block global_context "while.cond" fun_env.fun_def in
    let loopbb = Llvm.append_block global_context "while.loop" fun_env.fun_def in 
    let after_loop = Llvm.append_block global_context "while.after_loop" fun_env.fun_def in
    ignore(Llvm.build_br condbb fun_env.ibuilder);
    Llvm.position_at_end condbb fun_env.ibuilder;
    let guard_exp = eval_exp exp fun_env in
    ignore(Llvm.build_cond_br guard_exp loopbb after_loop fun_env.ibuilder);
    Llvm.position_at_end loopbb fun_env.ibuilder;
    let has_return = eval_stmt s1 fun_env in
    if has_return then () else ignore(Llvm.build_br condbb fun_env.ibuilder);
    Llvm.position_at_end after_loop fun_env.ibuilder;
    has_return

  | Ast.For(e1, e2, e3, for_body) ->
    let condbb = Llvm.append_block global_context "for.cond" fun_env.fun_def in
    let loopbb = Llvm.append_block global_context "for.loop" fun_env.fun_def in
    let after_loopbb = Llvm.append_block global_context "for.after_loop" fun_env.fun_def in 
    let _ = match e1 with None -> () | Some e1 -> ignore (eval_exp e1 fun_env) in 
    ignore(Llvm.build_br condbb fun_env.ibuilder);
    Llvm.position_at_end condbb fun_env.ibuilder;
    (* Inside condbb *)
    let _ = match e2 with
    | None -> ()
    | Some e2 -> 
      let e2 = eval_exp e2 fun_env in
      let _ = Llvm.build_cond_br e2 loopbb after_loopbb fun_env.ibuilder in ()
    in
    Llvm.position_at_end loopbb fun_env.ibuilder;
    (* Inside loopbb *)
    let has_return = eval_stmt for_body fun_env in
    let _ = match e3 with None -> () | Some e3 -> ignore (eval_exp e3 fun_env) in 
    if has_return then () else ignore(Llvm.build_br condbb fun_env.ibuilder);
    (* After loop body *)
    Llvm.position_at_end after_loopbb fun_env.ibuilder;
    has_return

  | Ast.Block(b) ->
    (* We are entering in a new inner block. *)
    (* Push a new block inside the symbol table. *)
    fun_env.fsym_table <- Symbol_table.begin_block fun_env.fsym_table;
    (* Emit block instructions *)
    let has_return = List.fold_left (fun acc s -> eval_stmtordec s fun_env || acc) false b in
    (* Pop the block from the symbol table since we are exiting from the block *)
    fun_env.fsym_table <- Symbol_table.end_block fun_env.fsym_table;
    has_return

  | Ast.Return(Some e) ->
    let ll_exp = (eval_exp e fun_env) in
    ignore(Llvm.build_ret ll_exp fun_env.ibuilder);
    true

  | Ast.Return(None) -> 
    ignore(Llvm.build_ret_void fun_env.ibuilder);
    true

and eval_stmtordec node fun_env = 
  match node.Ast.node with
  | Ast.LocalDecl((vid, vtyp), None) -> 
    (* Put llvalue inside the current block *)
    let llvalue = aux_build_alloca vid vtyp fun_env.ibuilder in 
    (* Update symbol table *)
    fun_env.fsym_table <- Symbol_table.add_entry vid llvalue fun_env.fsym_table;
    false
  | Ast.LocalDecl((vid, vtyp), Some exp) ->
    (* Put llvalue inside the current block *)
    let llvalue = aux_build_alloca vid vtyp fun_env.ibuilder in 
    (* Evaluate expression *)
    let llv_exp = eval_exp exp fun_env in
    (* Build store instruction. *)
    ignore(Llvm.build_store llv_exp llvalue fun_env.ibuilder);
    (* Update symbol table *)
    fun_env.fsym_table <- Symbol_table.add_entry vid llvalue fun_env.fsym_table;
    false
  | Ast.Stmt(s) -> eval_stmt s fun_env

and eval_member_decl node comp_env decl_st =
  let (cname, is_main_component, current_module) = (comp_env.cname, comp_env.is_main, comp_env.current_module) in
  match node.Ast.node with 
  | Ast.FunDecl({Ast.fname; Ast.rtype; Ast.formals; Ast.body = Some body; _}) ->
    (* Remove the old function declaration... *)
    let fun_name = if (is_main_component && fname = "main") then "main" else name_mangling cname fname in
    let _ = Llvm.delete_function (Symbol_table.lookup fun_name decl_st) in
    (* Define a new function environment *)
    let fun_st = Symbol_table.begin_block (Symbol_table.empty_table) in
    (* Define a new function from the current component. The function type is held by the node annotation. *)
    let fun_type = mucomp_type_to_llvm (node.Ast.annot) in
    let fun_def = Llvm.define_function fun_name fun_type current_module in
    (* Build a new instruction builder to generate instruction for the function *)
    let entrybb = Llvm.entry_block fun_def in
    let fun_ibuilder = Llvm.builder_at_end global_context entrybb in
    (* Allocate and store function paramaters inside the stack. *)
    let fun_st = List.fold_left (fun acc (idx, (vid, vtyp)) -> 
      let param_stack = aux_build_alloca vid vtyp fun_ibuilder in
      let param = Llvm.param fun_def idx in
      let _ = Llvm.build_store param param_stack fun_ibuilder in 
      Symbol_table.add_entry vid param_stack acc
    ) fun_st (Mcomp_stdlib.list_zip_with_index formals) in
    (* Evaluate statement. *)
    let fun_env = {
      fsym_table = fun_st; 
      fun_def = fun_def; 
      current_module = current_module;
      ibuilder = fun_ibuilder; 
    } in
    let _ = eval_stmt body fun_env in 
    let _ = remove_useless_returns fun_env in
    let _ = unify_blocks rtype fun_env in
    let _ = fill_missing_returns rtype fun_env in
    let _ = remove_empty_blocks fun_env in
    ()

  | Ast.VarDecl((vid, vtyp), None) -> 
    (* Remove the old variable declaration... *)
    let var_name = name_mangling cname vid in 
    let _ = Llvm.delete_global (Symbol_table.lookup var_name decl_st) in
    (* We initialize the variable with value equal to zero if possible *)
    let initialized_value = get_default_value vtyp in 
    let _ = Llvm.define_global var_name initialized_value current_module in ()

  | Ast.VarDecl((vid, vtyp), Some exp) -> 
    (* Remove the old variable declaration... *)
    let var_name = name_mangling cname vid in 
    let _ = Llvm.delete_global (Symbol_table.lookup var_name decl_st) in
    (* We initialize the variable with value equal to zero if possible *)
    let initialized_value = get_default_value vtyp in 
    let global_var = Llvm.define_global var_name initialized_value current_module in
    let builder = (snd comp_env.global_env.global_var_init) in
    let fun_env = {
      fsym_table = (Symbol_table.empty_table); 
      fun_def = (fst comp_env.global_env.global_var_init); 
      ibuilder = builder; 
      current_module = current_module;
    } in
    let global_value = eval_exp exp fun_env in
    ignore(Llvm.build_store global_value global_var builder);

  | _ -> ignore_pattern ()

and declare_component_members node cname is_main_component current_module acc = 
  match node.Ast.node with
  | Ast.FunDecl({Ast.fname; _}) ->
    (* Declare the function and put it inside the symbol table *)
    let fun_type = mucomp_type_to_llvm (node.Ast.annot) in
    let fun_name = if (is_main_component && fname = "main") then "main" else name_mangling cname fname in
    let fun_decl = Llvm.declare_function fun_name fun_type current_module in
    Symbol_table.add_entry fun_name fun_decl acc

  | Ast.VarDecl((vid, vtyp), _) -> 
    let var_lltype = mucomp_type_to_llvm vtyp in 
    let var_name = name_mangling cname vid in 
    let var_decl = Llvm.declare_global var_lltype var_name current_module in    
    Symbol_table.add_entry var_name var_decl acc

and eval_component_decl comp_node temp_st global_env =
  let current_module = global_env.global_module in
  match comp_node.Ast.node with
  | Ast.ComponentDecl({cname; provides; definitions; _}) ->
    let is_main_component = (List.mem "App" provides) in
    let comp_env = {cname = cname; is_main = is_main_component; current_module = current_module; global_env = global_env} in
    let _ = List.iter (fun m -> eval_member_decl m comp_env temp_st) definitions in ()

and declare_component node current_module acc =
  match node.Ast.node with
  | Ast.ComponentDecl({cname; provides; definitions; _}) ->
    let is_main_component = (List.mem "App" provides) in
    (* First declare the component members ... *)
    List.fold_left (fun acc m -> declare_component_members m cname is_main_component current_module acc) acc definitions

let emit_global_constructor current_module = 
    (* See: https://llvm.org/docs/LangRef.html#the-llvm-global-ctors-global-variable *)

    let lltype_void_function = Llvm.function_type void_type [||] in
    let lltype_ptr_void_function = Llvm.pointer_type lltype_void_function in
    let lltype_ptr_char = Llvm.pointer_type i8_type in

    let fname =  "_MUCOMP__global_ctors" in
    let ftyp = mucomp_type_to_llvm (Ast.TFun([], Ast.TVoid)) in
    let fun_priority = Llvm.const_int i32_type 65535 in
    let fun_decl = Llvm.define_function fname ftyp current_module in
    ignore(Llvm.set_linkage Llvm.Linkage.Internal fun_decl);
    let fbuilder = Llvm.builder_at_end global_context (Llvm.entry_block fun_decl) in
    let ret_inst = (Llvm.build_ret_void fbuilder) in

    let constructor_type = Llvm.struct_type global_context [|i32_type; lltype_ptr_void_function; lltype_ptr_char|] in 
    let global_init_constructor = Llvm.const_struct global_context [|fun_priority; fun_decl; Llvm.const_null lltype_ptr_char|] in
    let constructor_array = Llvm.const_array constructor_type [|global_init_constructor|] in 
    let global_ctors = Llvm.define_global "llvm.global_ctors" constructor_array current_module in
    ignore(Llvm.set_linkage Llvm.Linkage.Appending global_ctors);
    ignore(Llvm.position_before ret_inst fbuilder);
    (fun_decl, fbuilder)
    

let define_global_var_init (_, ctors_builder) current_module = 
  (* Declar function to initialize global "complex" variables *)
  let fname = "_MUCOMP__global_var_initializer" in 
  let ftyp = mucomp_type_to_llvm (Ast.TFun([], Ast.TVoid)) in
  let fun_decl = Llvm.define_function fname ftyp current_module in
  let fbuilder = Llvm.builder_at_end global_context (Llvm.entry_block fun_decl) in
  let ret_inst = (Llvm.build_ret_void fbuilder) in
  ignore(Llvm.build_call fun_decl [||] "" ctors_builder);
  (Llvm.position_before ret_inst fbuilder);
  (fun_decl, fbuilder)

let compile_code ast = 
  (* The module disposition is handled somewhere else... *)
  let global_module = Llvm.create_module global_context "global-module" in

  let ctors = emit_global_constructor global_module in
  let global_var_init = define_global_var_init ctors global_module in

  (* Declare prelude functions *)
  let _ = List.iter (fun (id, typ) -> let _ = Llvm.declare_function (name_mangling Mcomp_stdlib.g_PRELUDE_ID id) (mucomp_type_to_llvm typ) global_module in ()) Mcomp_stdlib.prelude_signature in
  (* Declare components *)
  let components = match ast with Ast.CompilationUnit(cu) -> cu.components in 
  let temp_st = List.fold_left (fun acc c -> declare_component c global_module acc) (Symbol_table.begin_block (Symbol_table.empty_table)) components in
  (* Define a function to initialize global data... *)
  let global_env = {global_module = global_module; global_ctors = ctors; global_var_init = global_var_init } in
  (* Define components *)
  let _ = List.iter (fun c -> eval_component_decl c temp_st global_env) components in
  Llvm.dump_module global_module;  (* Only for debug purposes. *)
  global_module

let to_llvm_module ast = 
  compile_code ast
