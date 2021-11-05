/**
 mComp parser specification 
 */
%{

  exception Syntax_error of string

  let parse_boolean_exp exp = 
      match exp with
      | Ast.BLiteral -> exp
      | Ast.BinaryOp(op, _, _) -> begin
                                    match op with
                                    | Ast.And     | Ast.Or  | Ast.Geq 
                                    | Ast.Greater | Ast.Leq | Ast.Less
                                    | Ast.Neq     | Ast.Equal           -> exp
                                    | _ -> raise (Syntax_error "Expected a boolean condition...")
                                  end
      | _ -> raise (Syntax_error "Expected a boolean condition...")
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
%right    O_ASSIGN
%left     L_OR_OR
%left     L_AND_AND
%left     C_EQ_EQ C_NOT_EQ
%nonassoc C_LT C_GT C_GT_EQ C_LT_EQ
%left     M_PLUS M_MINUS
%left     M_TIMES M_DIV M_MOD
%nonassoc L_NOT O_REF
%nonassoc L_SQUARE DOT


/* Start symbol */
%start compilation_unit
%type <Ast.located_compilation_unit> compilation_unit 

%% 


/* Grammar Specification */

compilation_unit:
  | tl = top_decl; EOF       
    { 
      failwith "Not implemented yet"   
    }                   
;

top_decl:
  | K_INTERFACE; id = ID; l = delimited(L_BRACKET, nonempty_list(i_member_decl), R_BRACKET) {}
  | K_COMPONENT; id = ID; p = option(provide_clause); u = option(use_clause); members = delimited(L_BRACKET, nonempty_list(c_member_decl), R_BRACKET) {

  }
  | K_CONNECT; l = link; SEMICOLON {}
  | K_CONNECT; links = delimited(L_BRACKET, separated_nonempty_list(SEMICOLON, link), R_BRACKET) {}
 ; 

link:
  | c1 = ID; DOT; m1 = ID; LINK; c2 = ID; DOT; m2 = ID {}
;

i_member_decl:
  | K_VAR; vs = var_sign; SEMICOLON {}
  | fp = fun_proto; SEMICOLON {}
;

provide_clause:
  | K_PROVIDES; l = separated_nonempty_list(COMMA, ID); el = ID {}
;

use_clause:
  | K_USES; l = separated_nonempty_list(COMMA, ID); el = ID {}
;

var_sign:
  | id = ID COLON t = type {}
;

fun_proto:
  | K_DEF; id = ID; p = delimited(L_PAREN, option(l = separated_nonempty_list(COMMA, var_sign); el = var_sign), R_PAREN); COLON; bt = option(basic_type) {

  }
;

c_member_decl:
  | K_VAR; vs = var_sign; SEMICOLON {}
  | fd = fun_decl {}
;

fun_decl:
  | f = fun_proto; b = block {}
;

block:
  | l = delimited(L_BRACKET, list(block_content), R_BRACKET) {}
;

block_content:
  | s = stmt { s }
  | K_VAR; vs = var_sign; SEMICOLON { vs }
;

type:
  | bt = basic_type { bt }

  | t = type L_SQUARE R_SQUARE { Ast.TArray(t, None) }

  | t = type; size = delimited(L_SQUARE, INT, R_SQUARE) { Ast.TArray(t, size) }

  | O_REF; t = basic_type { Ast.TRef(t) }
;

basic_type:
  | K_INT   { Ast.TInt }
  | K_CHAR  { Ast.TChar }
  | K_BOOL  { Ast.TBool }
  | K_VOID  { Ast.TVoid }
;

(* TOASK: skip? missing for? missing option for if? *)
stmt: 
  | exp = delimited(K_RETURN, expr, SEMICOLON) { Ast.Return(exp)}

  | exp = expr SEMICOLON { Ast.Exp(exp) }

  (* TODO *)
  | b = block {}

  | K_WHILE; exp = delimited(L_PAREN, expr, R_PAREN); s = stmt {
    try
      Ast.While(parse_boolean_exp exp, s)
    with Syntax_error(msg) -> 
      failwith msg
  }

  | K_IF; exp = delimited(L_PAREN, expr, R_PAREN); s1 = stmt; K_ELSE s2 = stmt {
    try
      Ast.If(parse_boolean_exp exp, s1, s2)
    with Syntax_error(msg) -> 
      failwith msg
  }

  | K_IF exp = delimited(L_PAREN, expr, R_PAREN); s = stmt {
        try
          Ast.If(parse_boolean_exp exp, s, None)
        with Syntax_error(msg) -> 
          failwith msg
  }
;

expr:
  | num = INT   { Ast.ILiteral(num) }
  | chr = CHAR  { Ast.CLiteral(chr) }
  | bol = BOOL  { Ast.BLiteral(bol) }

  | exp = delimited(L_PAREN, expr, R_PAREN) { exp }

  | O_REF; lv = l_value { Ast.Address(lv) }

  | lv = l_value; O_ASSIGN; exp = expr { Ast.Assign(lv, exp) }

  (*TODO: | function call*)
  
  | lv = l_value { Ast.LV(lv) }
  
  (*TODO: | unary *) 
  (* | L_NOT exp = expr {} *)

  (* TODO: match types? *)
  | e1 = expr; op = bin_op; e2 = expr { Ast.BinaryOp(op, e1, e2) }
;

l_value:
  | id = ID { Ast.AccVar(None, id) } (* FIXME? *)
  | id = ID idx = delimited(L_SQUARE, expr, R_SQUARE) { Ast.AccIndex(id, idx) } (* arr[int_expr] *)
;

bin_op: 
  | M_PLUS    { Ast.Add  }
  | M_MINUS   { Ast.Sub  }
  | M_TIMES   { Ast.Mult }
  | M_MOD     { Ast.Mod  }
  | M_DIV     { Ast.Div  }
  | L_AND_AND { Ast.And  }
  | L_OR_OR   { Ast.Or   }
  | C_LT      { Ast.Less }
  | C_GT      { Ast.Greater }
  | C_LT_EQ   { Ast.Leq } 
  | C_GT_EQ   { Ast.Geq }
  | C_EQ_EQ   { Ast.Equal }
;