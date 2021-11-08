/**
 mComp parser specification 
 */
%{

%} 

/* Token declarations */
%token EOF

%token <int> INT
%token <char> CHAR 
%token <bool> BOOL

%token <string> ID

(* Keywords *)

%token K_VAR
%token K_DEF
%token K_USES
%token K_CONNECT
%token K_PROVIDES
%token K_INTERFACE
%token K_COMPONENT

%token K_INT
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

(* Math operators *)
%token M_PLUS
%token M_MINUS
%token M_TIMES
%token M_DIV
%token M_MOD

(* Other operators *)
%token O_ASSIGN
%token O_REF

(* Symbols *)
%token L_PAREN
%token R_PAREN

%token L_BRACKET
%token R_BRACKET

%token L_SQUARE
%token R_SQUARE

%token DOT
%token COMMA
%token COLON
%token SEMICOLON
%token LINK

/* Precedence and associativity specification */
%nonassoc  K_IF
%nonassoc  K_ELSE

%right    O_ASSIGN

%left     L_OR_OR
%left     L_AND_AND
%left     C_EQ_EQ C_NOT_EQ
%nonassoc C_LT C_GT C_GT_EQ C_LT_EQ

%left     M_PLUS M_MINUS
%left     M_TIMES M_DIV M_MOD

%nonassoc U_MINUS U_NOT

%nonassoc O_REF
%nonassoc L_SQUARE


/* Start symbol */
%start compilation_unit
%type <Ast.located_compilation_unit> compilation_unit 

%% 


/* Grammar Specification */
compilation_unit:
  | ifaces = list(interface); comps = list(component); cons = connection EOF { 
      Ast.CompilationUnit({interfaces = ifaces; components = comps; connections = cons})
  }                   
;

interface:
  | K_INTERFACE; iface_name = ID; L_BRACKET; decls = nonempty_list(i_member_decl); R_BRACKET {
    Ast.make_node (Ast.InterfaceDecl({iname = iface_name; declarations = decls})) (Location.to_code_position $loc)
  }
;

component:
  | K_COMPONENT; comp_name = ID; pc = option(provide_clause); uc = option(use_clause); L_BRACKET; members = nonempty_list(c_member_decl); R_BRACKET {
    let uses_decl = match uc with None -> [] | Some(c) -> c in 
    let provides_decl = match pc with None -> [] | Some(p) -> p in
    Ast.make_node (Ast.ComponentDecl({cname = comp_name; uses = uses_decl; provides = provides_decl; definitions = members })) (Location.to_code_position $loc)
  }

connection:
  | K_CONNECT; l = link; SEMICOLON {
    [l]
  }
  | K_CONNECT; L_BRACKET; links = separated_list(SEMICOLON, link); R_BRACKET {
    links
  }
;


link:
  | c1 = ID; DOT; m1 = ID; LINK; c2 = ID; DOT; m2 = ID {
    Ast.Link(c1, m1, c2, m2)
  }
;

i_member_decl:
  | K_VAR; vs = var_sign; SEMICOLON {
    Ast.make_node (Ast.VarDecl(vs)) (Location.to_code_position $loc)
  }
  | fp = fun_proto; SEMICOLON {
    Ast.make_node (Ast.FunDecl(fp)) (Location.to_code_position $loc)
  }
;

provide_clause:
  | K_PROVIDES; l = separated_list(COMMA, ID); {
    l
  }
;

use_clause:
  | K_USES; l = separated_list(COMMA, ID); {
    l
  }
;

var_sign:
  | id = ID; COLON; t = lang_type { (id, t) }
;

fun_proto:
  | K_DEF; fn_name = ID; L_PAREN; fp = separated_list(COMMA, var_sign); R_PAREN; COLON; bt = option(basic_type) {
      match bt with 
      | None -> Ast.make_fun_decl Ast.TVoid fn_name fp None
      | Some(t) -> Ast.make_fun_decl t fn_name fp None
  }
;

c_member_decl:
  | K_VAR; vs = var_sign; SEMICOLON; {
    Ast.make_node (Ast.VarDecl(vs)) (Location.to_code_position $loc)
  }
  | fd = fun_decl {
    Ast.make_node (Ast.FunDecl(fd)) (Location.to_code_position $loc)
  }
;

(* TODO: fix body *)
fun_decl:
  | fun_proto; block {
    Ast.make_fun_decl (Ast.TInt) ("temp") ([]) (None)
  }
;

block:
  | L_BRACKET; l = list(block_element); R_BRACKET {
    Ast.make_node (Ast.Block(l)) (Location.to_code_position $loc)
  }
;

block_element:
  | s = stmt { Ast.make_node (Ast.Stmt(s)) (Location.to_code_position $loc) }
  | K_VAR; vs = var_sign; SEMICOLON; { Ast.make_node (Ast.LocalDecl(vs)) (Location.to_code_position $loc) }
;

lang_type:
  | t = basic_type { t }
  | t = lang_type; L_SQUARE; R_SQUARE { Ast.TArray(t, None) }
  | t = lang_type; L_SQUARE; size = INT; R_SQUARE { Ast.TArray(t, Some size) }
  | O_REF; t = lang_type; %prec O_REF { Ast.TRef(t) }
;

basic_type:
  | K_INT   { Ast.TInt  }
  | K_CHAR  { Ast.TChar }
  | K_VOID  { Ast.TVoid }
  | K_BOOL  { Ast.TBool }
;

stmt:
  | SEMICOLON {
    Ast.make_node (Ast.Skip) (Location.to_code_position $loc)
  }
  | K_RETURN; e = expr; SEMICOLON {
    Ast.make_node (Ast.Return(Some e)) (Location.to_code_position $loc)
  }
  | K_RETURN; SEMICOLON {
    Ast.make_node (Ast.Return(None)) (Location.to_code_position $loc)
  }
  | e = expr; SEMICOLON {
    Ast.make_node (Ast.Expr(e)) (Location.to_code_position $loc)
  }
  | K_WHILE; e = delimited(L_PAREN, expr, R_PAREN); s = stmt {
    Ast.make_node (Ast.While(e, s)) (Location.to_code_position $loc)
  }
  | K_IF; e = delimited(L_PAREN, expr, R_PAREN); s = stmt %prec K_IF {
    Ast.make_node (Ast.If(e, s, None)) (Location.to_code_position $loc)
  }
  | K_IF; e = delimited(L_PAREN, expr, R_PAREN); s1 = stmt; K_ELSE; s2 = stmt {
    Ast.make_node (Ast.If(e, s1, Some s2)) (Location.to_code_position $loc)
  }
;

expr:
  | n = INT { Ast.make_node (Ast.ILiteral(n)) (Location.to_code_position $loc) }
  | b = BOOL { Ast.make_node (Ast.BLiteral(b)) (Location.to_code_position $loc) }
  | c = CHAR { Ast.make_node (Ast.CLiteral(c)) (Location.to_code_position $loc) }
  | e = delimited(L_PAREN, expr, R_PAREN) { e }
 
  | lv = l_value; {
    Ast.make_node (Ast.LV(lv)) (Location.to_code_position $loc)
  }
  | lv = l_value; O_ASSIGN; e = expr {
    Ast.make_node (Ast.Assign(lv, e)) (Location.to_code_position $loc)
  }
  | O_REF; lv = l_value; {
    Ast.make_node (Ast.Address(lv)) (Location.to_code_position $loc)
  }

  | M_MINUS; e = expr; %prec U_MINUS {
    Ast.make_node (Ast.UnaryOp(Ast.Neg, e)) (Location.to_code_position $loc)
  }
  | L_NOT; e = expr; %prec U_NOT {
    Ast.make_node (Ast.UnaryOp(Ast.Not, e)) (Location.to_code_position $loc)
  }

  | e1 = expr; M_PLUS; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Add, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; M_MINUS; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Sub, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; M_TIMES; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Mult, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; M_DIV; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Div, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; M_MOD; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Mod, e1, e2)) (Location.to_code_position $loc)
  }

  | e1 = expr; L_AND_AND; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.And, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; L_OR_OR; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Or, e1, e2)) (Location.to_code_position $loc)
  }

  | e1 = expr; C_LT; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Less, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; C_GT; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Greater, e1, e2)) (Location.to_code_position $loc)
  }

  | e1 = expr; C_LT_EQ; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Leq, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; C_GT_EQ; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Geq, e1, e2)) (Location.to_code_position $loc)
  }

  | e1 = expr; C_EQ_EQ; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Equal, e1, e2)) (Location.to_code_position $loc)
  }
  | e1 = expr; C_NOT_EQ; e2 = expr {
    Ast.make_node (Ast.BinaryOp(Ast.Neq, e1, e2)) (Location.to_code_position $loc)
  }
;

l_value:
  | id = ID { Ast.make_node (Ast.AccVar(None, id)) (Location.to_code_position $loc) }
;