type binop =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Equal
  | Neq
  | Less
  | Leq
  | Greater
  | Geq
  | And
  | Or
[@@deriving show, ord, eq]

type uop = Neg | Not [@@deriving show, ord, eq]
type dop = MinMin | PlusPlus and dop_prec = Pre | Post [@@deriving show, ord, eq]

type identifier = string [@@deriving show, ord, eq]

type typ =
  | TInt (* Type int *)
  | TFloat (* Type float *)
  | TBool (* Type bool *)
  | TChar (* Type char *)
  | TArray of typ * int option (* Array type *)
  | TRef of typ (* Reference type *)
  | TVoid (* Type unit *)
  | TFun of typ list * typ (* Type functions [paramerts] -> return_type *)
  | TInterface of identifier  (* Type of an interface *)
  | TComponent of identifier (* Type of a component *)
[@@deriving show, ord, eq]

type ('a, 'b) annotated_node = { node : 'a; annot : 'b }
[@@deriving show, ord, eq]

type vdecl = identifier * typ [@@deriving show, ord, eq]

type 'a expr = ('a expr_node, 'a) annotated_node

and 'a expr_node =
  | LV of 'a lvalue (* `x or a[e] *)
  | Assign of 'a lvalue * 'a expr (* x=e or a[e]=e *)
  | ILiteral of int (* Integer literal *)
  | FLiteral of float (* Float literal *)
  | CLiteral of char (* Char literal *)
  | BLiteral of bool (* Bool literal *)
  | UnaryOp of uop * 'a expr (* Unary primitive operator *)
  | DoubleOp of dop * dop_prec * 'a lvalue 
  | Address of 'a lvalue (* Address of a variable *)
  | BinaryOp of binop * 'a expr * 'a expr (* Binary primitive operator *)
  | Call of identifier option * identifier * 'a expr list
(* Function call f(...) *)
[@@deriving show, ord, eq]

and 'a lvalue = ('a lvalue_node, 'a) annotated_node

and 'a lvalue_node =
  | AccVar of identifier option * identifier (* Variable access x *)
  | AccIndex of 'a lvalue * 'a expr
(* Array indexing a[e] *)
[@@deriving show, ord, eq]

and 'a stmt = ('a stmt_node, 'a) annotated_node

and 'a stmt_node =
  | If of 'a expr * 'a stmt * 'a stmt (* Conditional *)
  | While of 'a expr * 'a stmt (* While loop *)
  | For of 'a expr option * 'a expr option * 'a expr option * 'a stmt (* For loop *)
  | Expr of 'a expr (* Expression statement e; *)
  | Return of 'a expr option (* Return statement *)
  | Block of 'a stmtordec list (* Block: grouping and scope *)
  | Skip
[@@deriving show, ord, eq]

and 'a stmtordec = ('a stmtordec_node, 'a) annotated_node

and 'a stmtordec_node =
  | LocalDecl of vdecl * 'a expr option (* Local variable declaration and an optional initialization *)
  | Stmt of 'a stmt (* A statement *)
[@@deriving show, ord, eq]

and 'a fun_decl = { 
  rtype : typ;
  fname : identifier;
  formals : vdecl list;
  body : 'a stmt option; (* None when is a prototype *)
}
[@@deriving show, ord, eq]

and 'a member_decl = ('a member_decl_node, 'a) annotated_node
[@@deriving show, ord, eq]

and 'a member_decl_node =
  (* A member of an interface or of a component *)
  | FunDecl of 'a fun_decl
  | VarDecl of vdecl * 'a expr option
[@@deriving show, ord, eq]

and 'a interface_decl = ('a interface_decl_node, 'a) annotated_node 

and 'a interface_decl_node =
  | InterfaceDecl of {
      (* Interface declaration *)
      iname : identifier;
      declarations : 'a member_decl list;    
    }
[@@deriving show, ord, eq]

and 'a component_decl = ('a component_decl_node, 'a) annotated_node

and 'a component_decl_node =
  | ComponentDecl of {
      (* Component declaration *)
      cname : identifier;
      uses : identifier list;
      provides : identifier list;
      definitions : 'a member_decl list;
    }
[@@deriving show, ord, eq]

and connection = Link of identifier * identifier * identifier * identifier
[@@deriving show, ord, eq]

and 'a definition = 
  | ComponentDef of 'a component_decl 
  | InterfaceDef of 'a interface_decl
  | ConnectionDef of connection list

and 'a compilation_unit =
  | CompilationUnit of {
      interfaces : 'a interface_decl list;
      components : 'a component_decl list;
      connections : connection list;
    }
[@@deriving show, ord, eq]

type located_compilation_unit = Location.code_pos compilation_unit
[@@deriving show, ord, eq]

type typed_compilation_unit = typ compilation_unit
[@@deriving show, ord, eq]

let make_node a b = { node = a; annot = b}

let base_typ = function 
  | TArray(t, _) -> t
  | TRef(t) -> t 
  | _ as t -> t

let rec is_scalar_type = function
  | TInt | TChar | TBool | TFloat -> true
  | TRef(_) as t -> is_ref_to_scalar_type t
  | _ -> false
and is_ref_to_scalar_type = function 
  | TRef(t) -> is_scalar_type t 
  | _ -> false

let is_math_operator = function 
  | Add | Sub | Mult | Div | Mod -> true 
  | _ -> false

let is_bool_operator = function 
  | And | Or -> true 
  | _ -> false

let is_compare_operator = function 
  | Greater | Less | Geq | Leq -> true 
  | _ -> false

let rec show_type = function 
  | TInt            -> "int"
  | TChar           -> "char"
  | TBool           -> "bool"
  | TVoid           -> "void"
  | TFloat          -> "float"
  | TRef(t)         -> "&" ^ (show_type t)  (* &type *)
  | TArray(t, _)    -> (show_type t) ^ "[]" (* type[] *)
  | TComponent(id)  -> ("C" ^ id) (* C<component_name> *)
  | TInterface(id)  -> ("I" ^ id) (* I<interface_name> *)
  | _ -> "undefined_type"

let make_fun_decl rtype fname formals body = {
  rtype = rtype; fname = fname; formals = formals; body = body
}

