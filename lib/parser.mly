/**
 mComp parser specification 
 */
%{

  (* Define a new operator `@>` used to attach the location as an annotation for an annotated node. *)
  let (@>) a b = Ast.make_node a (Location.to_code_position b)

  let build_compilation_unit decls =
    let rec aux interfaces components connections = function
      | [] -> Ast.CompilationUnit({interfaces = interfaces; components = components; connections = connections})
      | (Ast.InterfaceDef(iface_node))::t -> aux (iface_node::interfaces) components connections t
      | (Ast.ComponentDef(comp_node))::t -> aux interfaces (comp_node::components) connections t
      | (Ast.ConnectionDef(conns)::t) -> aux interfaces components (conns@connections) t
    in aux [] [] [] decls
  
%} 

/* Token declarations */
%token EOF

%token <string> ID
%token <int> INT
%token <float> FLOAT
%token <char> CHAR 
%token <bool> BOOL

(* Keywords *)

%token K_VAR
%token K_DEF
%token K_USES
%token K_CONNECT
%token K_PROVIDES
%token K_INTERFACE
%token K_COMPONENT

%token K_INT
%token K_FLOAT
%token K_CHAR
%token K_BOOL
%token K_VOID

(* Flow control *)
%token K_FOR
%token K_WHILE

%token K_IF
%token K_ELSE

%token K_RETURN

(* Logical operators *)
%token L_NOT
%token L_AND_AND
%token L_OR_OR

(* Comparsion operators *)
%token C_LT
%token C_GT
%token C_EQ_EQ
%token C_GT_EQ
%token C_LT_EQ
%token C_NOT_EQ

%token M_PLUS_PLUS
%token M_MINUS_MINUS

(* Math operators *)
%token M_PLUS
%token M_MINUS
%token M_TIMES
%token M_DIV
%token M_MOD

%token O_PLUS_ASSIGN  // +=
%token O_MINUS_ASSIGN // -=
%token O_TIMES_ASSIGN // *=
%token O_DIV_ASSIGN   // /=
%token O_MOD_ASSIGN   // %=

(* Other operators *)
%token O_ASSIGN
%token O_REF

(* Symbols *)
%token L_PAREN R_PAREN // `(`, `)`

%token L_BRACKET R_BRACKET // `{`, `}`

%token L_SQUARE R_SQUARE // `[`, `]`

%token DOT
%token COMMA
%token COLON
%token SEMICOLON
%token LINK

/* Precedence and associativity specification */
%nonassoc  K_IF
%nonassoc  K_ELSE

%right    O_ASSIGN O_PLUS_ASSIGN O_MINUS_ASSIGN O_TIMES_ASSIGN O_DIV_ASSIGN O_MOD_ASSIGN

%left     L_OR_OR
%left     L_AND_AND
%left     C_EQ_EQ C_NOT_EQ
%nonassoc C_LT C_GT C_GT_EQ C_LT_EQ

%left     M_PLUS M_MINUS
%left     M_TIMES M_DIV M_MOD

%right M_MINUS_MINUS

%nonassoc U_MINUS
%nonassoc U_NOT

/* Start symbol */
%start compilation_unit
%type <Ast.located_compilation_unit> compilation_unit 

%% 

/* Grammar Specification */
compilation_unit:
  | decls = top_decl* EOF { build_compilation_unit decls }
;

top_decl:
  | interface   { Ast.InterfaceDef($1) }
  | component   { Ast.ComponentDef($1) }
  | connections { Ast.ConnectionDef($1) }
;

interface:
  | K_INTERFACE iface_name = ID decls = delimited(L_BRACKET, i_member_decl+, R_BRACKET) {
    (Ast.InterfaceDecl({iname = iface_name; declarations = decls})) @> $loc
  }
;

component:
  | K_COMPONENT comp_name = ID pc = provide_clause? uc = use_clause? members = delimited(L_BRACKET, c_member_decl+, R_BRACKET) {
    let uses_decl =     match uc with None -> [] | Some(c) -> c in 
    let provides_decl = match pc with None -> [] | Some(p) -> p in
    (Ast.ComponentDecl({cname = comp_name; uses = uses_decl; provides = provides_decl; definitions = members })) @> $loc
  }
;

connections:
  | K_CONNECT l = link SEMICOLON? { [l] }
  | K_CONNECT ls = delimited(L_BRACKET, links, R_BRACKET) { ls }
;

links:
  | {[]}
  | l = link SEMICOLON ls = links { l::ls }
;

link:
  | c1 = ID DOT m1 = ID LINK c2 = ID DOT m2 = ID { Ast.Link(c1, m1, c2, m2) }
;

i_member_decl:
  | K_VAR vs = var_sign SEMICOLON { (Ast.VarDecl(vs, None)) @> $loc }
  | fp = fun_proto SEMICOLON { (Ast.FunDecl(fp)) @> $loc }
;

provide_clause:
  | K_PROVIDES l = separated_list(COMMA, ID) { l }
;

use_clause:
  | K_USES l = separated_list(COMMA, ID) { l }
;


var_sign:
  | id = ID COLON t = lang_type { (id, t) }
;

fun_proto:
  | K_DEF fname = ID L_PAREN fp = separated_list(COMMA, var_sign) R_PAREN {
    Ast.make_fun_decl Ast.TVoid fname fp None
  }
  | K_DEF fname = ID L_PAREN fp = separated_list(COMMA, var_sign) R_PAREN COLON bt = basic_type {
    Ast.make_fun_decl bt fname fp None
  }
  | error {
    failwith "An error occurred when the parser was analyzing a function signature!"
  }
;

c_member_decl:
  | K_VAR vs = var_sign SEMICOLON { (Ast.VarDecl(vs, None)) @> $loc }
  | K_VAR vs = var_sign O_ASSIGN e = expr SEMICOLON { (Ast.VarDecl(vs, Some e)) @> $loc }
  | fd = fun_decl { (Ast.FunDecl(fd)) @> $loc }
;

fun_decl:
  | fp = fun_proto b = block {
    Ast.make_fun_decl (fp.Ast.rtype) (fp.Ast.fname) (fp.Ast.formals) (Some b)
  }
;

block:
  | elements = delimited(L_BRACKET, block_element*, R_BRACKET) {
    (Ast.Block(elements)) @> $loc
  }
;

block_element:
  | s = stmt { (Ast.Stmt(s)) @> $loc }
  | K_VAR vs = var_sign SEMICOLON { (Ast.LocalDecl(vs, None)) @> $loc }
  | K_VAR vs = var_sign O_ASSIGN e = expr SEMICOLON { (Ast.LocalDecl(vs, Some e)) @> $loc }
;

lang_type:
  | t = basic_type { t }
  | t = lang_type L_SQUARE R_SQUARE { Ast.TArray(t, None) }
  | t = lang_type size = delimited(L_SQUARE, INT, R_SQUARE) { Ast.TArray(t, Some size) }
  | O_REF t = basic_type { Ast.TRef(t) }
;

basic_type:
  | K_INT   { Ast.TInt  }
  | K_CHAR  { Ast.TChar }
  | K_VOID  { Ast.TVoid }
  | K_BOOL  { Ast.TBool }
  | K_FLOAT { Ast.TFloat }
;

stmt:
  | SEMICOLON {
    (Ast.Skip) @> $loc
  }
  | K_RETURN e = expr? SEMICOLON {
    (Ast.Return(e)) @> $loc
  }
  | e = expr SEMICOLON {
    (Ast.Expr(e)) @> $loc
  }
  | b = block { b }
  | K_WHILE e = delimited(L_PAREN, expr, R_PAREN) s = stmt {
    (Ast.While(e, s)) @> $loc
  }
  | K_IF e = delimited(L_PAREN, expr, R_PAREN) s = stmt %prec K_IF {
    let skip = Ast.Skip @> $loc in 
    (Ast.If(e, s, skip)) @> $loc
  }
  | K_IF e = delimited(L_PAREN, expr, R_PAREN) s1 = stmt K_ELSE s2 = stmt {
    (Ast.If(e, s1, s2)) @> $loc
  }
  | K_FOR L_PAREN e1 = expr? SEMICOLON e2 = expr? SEMICOLON e3 = expr? R_PAREN s = stmt {
    (Ast.For(e1, e2, e3, s)) @> $loc
  }
;

expr:
  | n = INT   { (Ast.ILiteral(n)) @> $loc }
  | n = FLOAT   { (Ast.FLiteral(n)) @> $loc }
  | b = BOOL  { (Ast.BLiteral(b)) @> $loc}
  | c = CHAR  { (Ast.CLiteral(c)) @> $loc }
  | e = delimited(L_PAREN, expr, R_PAREN) { e }

  | O_REF lv = l_value {
    (Ast.Address(lv)) @> $loc
  }
  | lv = l_value O_ASSIGN e = expr {
    (Ast.Assign(lv, e)) @> $loc
  }
  | lv = l_value O_PLUS_ASSIGN e = expr {
    let lv_exp = Ast.LV(lv) @> $loc in
    let final_exp = (Ast.BinaryOp(Ast.Add, lv_exp, e)) @> $loc in 
    (Ast.Assign(lv, final_exp)) @> $loc
  }
  | lv = l_value O_MINUS_ASSIGN e = expr {
    let lv_exp = Ast.LV(lv) @> $loc in
    let final_exp = (Ast.BinaryOp(Ast.Sub, lv_exp, e)) @> $loc in 
    (Ast.Assign(lv, final_exp)) @> $loc
  }
  | lv = l_value O_TIMES_ASSIGN e = expr {
    let lv_exp = Ast.LV(lv) @> $loc in
    let final_exp = (Ast.BinaryOp(Ast.Mult, lv_exp, e)) @> $loc in 
    (Ast.Assign(lv, final_exp)) @> $loc
  }
  | lv = l_value O_DIV_ASSIGN e = expr {
    let lv_exp = Ast.LV(lv) @> $loc in
    let final_exp = (Ast.BinaryOp(Ast.Div, lv_exp, e)) @> $loc in 
    (Ast.Assign(lv, final_exp)) @> $loc
  }
  | lv = l_value O_MOD_ASSIGN e = expr {
    let lv_exp = Ast.LV(lv) @> $loc in
    let final_exp = (Ast.BinaryOp(Ast.Mod, lv_exp, e)) @> $loc in 
    (Ast.Assign(lv, final_exp)) @> $loc
  }

  | M_MINUS e = expr %prec U_MINUS {
    (Ast.UnaryOp(Ast.Neg, e)) @> $loc
  }
  | L_NOT e = expr %prec U_NOT {
    (Ast.UnaryOp(Ast.Not, e)) @> $loc
  }

  | fname = ID exp_list = delimited(L_PAREN, separated_list(COMMA, expr), R_PAREN) {
    (Ast.Call(None, fname, exp_list)) @> $loc
  }

  | M_PLUS_PLUS l_value {
    (Ast.DoubleOp(Ast.PlusPlus, Ast.Pre, $2)) @> $loc
  }
  | l_value M_PLUS_PLUS {
    (Ast.DoubleOp(Ast.PlusPlus, Ast.Post, $1)) @> $loc
  }
  | M_MINUS_MINUS e = expr {
      let u_exp = Ast.UnaryOp(Ast.Neg, e) @> $loc in
      (Ast.UnaryOp(Ast.Neg, u_exp)) @> $loc
  }
  | l_value M_MINUS_MINUS {
    (Ast.DoubleOp(Ast.MinMin, Ast.Post, $1)) @> $loc
  }

  | lv = l_value {
    (Ast.LV(lv)) @> $loc
  }

  | e1 = expr M_PLUS e2 = expr {
    (Ast.BinaryOp(Ast.Add, e1, e2)) @> $loc
  }
  | e1 = expr M_MINUS e2 = expr {
    (Ast.BinaryOp(Ast.Sub, e1, e2)) @> $loc
  }
  | e1 = expr M_TIMES e2 = expr {
    (Ast.BinaryOp(Ast.Mult, e1, e2)) @> $loc
  }
  | e1 = expr M_DIV e2 = expr {
    (Ast.BinaryOp(Ast.Div, e1, e2)) @> $loc
  }
  | e1 = expr M_MOD e2 = expr {
    (Ast.BinaryOp(Ast.Mod, e1, e2)) @> $loc
  }

  | e1 = expr L_AND_AND e2 = expr {
    (Ast.BinaryOp(Ast.And, e1, e2)) @> $loc
  }
  | e1 = expr L_OR_OR e2 = expr {
    (Ast.BinaryOp(Ast.Or, e1, e2)) @> $loc
  }

  | e1 = expr C_LT e2 = expr {
    (Ast.BinaryOp(Ast.Less, e1, e2)) @> $loc
  }
  | e1 = expr C_GT e2 = expr {
    (Ast.BinaryOp(Ast.Greater, e1, e2)) @> $loc
  }

  | e1 = expr C_LT_EQ e2 = expr {
    (Ast.BinaryOp(Ast.Leq, e1, e2)) @> $loc
  }
  | e1 = expr C_GT_EQ e2 = expr {
    (Ast.BinaryOp(Ast.Geq, e1, e2)) @> $loc
  }

  | e1 = expr C_EQ_EQ e2 = expr {
    (Ast.BinaryOp(Ast.Equal, e1, e2)) @> $loc
  }
  | e1 = expr C_NOT_EQ e2 = expr {
    (Ast.BinaryOp(Ast.Neq, e1, e2)) @> $loc
  }

  | error {
    failwith "An error occurred when the parser was analyzing an expression!"
  }
;

l_value:
  | id = ID { (Ast.AccVar(None, id)) @> $loc }
  | id = ID L_SQUARE e = expr R_SQUARE {
    let n = (Ast.AccVar(None, id)) @> $loc in
    (Ast.AccIndex(n, e)) @> $loc
  }
;
