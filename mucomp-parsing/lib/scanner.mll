{
  (* OCaml declaration*)   
  open Parser
  exception Lexing_error of Lexing.position * string

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keywords_table = create_hashtable 16 [
    ("var",       K_VAR);
    ("def",       K_DEF);
    ("uses",      K_USES);
    ("int",       K_INT);
    ("char",      K_CHAR);
    ("bool",      K_BOOL);
    ("void",      K_VOID);
    ("for",       K_FOR);
    ("while",     K_WHILE);
    ("if",        K_IF);
    ("else",      K_ELSE);
    ("return",    K_RETURN);
    ("connect",   K_CONNECT);
    ("provides",  K_PROVIDES);
    ("interface", K_INTERFACE);
    ("component", K_COMPONENT);
  ]

  let max_id_length = 64

  (*
    Check if the identifier length is no more than 64 characters,
    otherwise raise a Lexing_error exception.
  *)
  let check_identifier id pos = 
    if String.length id < max_id_length then 
      ID(id) 
    else 
      let msg = Printf.sprintf "The identifier \"%s\" must be < %d characters!\n" id max_id_length in
      raise (Lexing_error (pos, msg))
  
  let check_num_int32 num pos =
    if num > max_int || num < min_int then
      let msg = Printf.sprintf "The number \"%d\" is not a 32 bit integer!\n" num in 
      raise (Lexing_error (pos, msg))
    else
      INT(num)
  
}

(* Declaration of regular expressions *)

let digit = ['0' - '9']
let dec_number = ['-']?digit+
let hex_number = ['-']?"0x"['0' - '9' 'a' - 'f' 'A' - 'F']+

let identifier = ['_']?['a' - 'z' 'A' - 'Z']['0' - '9' 'a' - 'z' 'A' - 'Z']*

(* TODO: handle escape characters *)
let characters = 
  ['\'']['a' - 'z' 'A' - 'Z' '0' - '9']['\'']

(* Declaration of scanner functions *)


rule next_token = parse

  (* Primitives *)
  | hex_number
  | dec_number as num   { 
                          let n = int_of_string num in 
                          check_num_int32 n (Lexing.lexeme_start_p lexbuf)
                        }

  | identifier as id    { 
                          try
                            let token = Hastbl.find keywords_table id in
                            token
                          with Not_found ->
                            check_identifier id (Lexing.lexeme_start_p lexbuf)
                        }
  | characters as c     { CHAR(c) }
  
  | "true"              { BOOL(true) }
  | "false"             { BOOL(false) }

  (* Logical operators *)
  | ['!']               { L_NOT }
  | "&&"                { L_AND_AND }
  | "||"                { L_OR_OR }

  (* Comparsion operators *)
  | '>'                 { C_GT }
  | '<'                 { C_LT }
  | "=="                { C_EQ_EQ}
  | "!="                { C_NOT_EQ}
  | ">="                { C_GT_EQ}
  | "<="                { C_LT_EQ}

  (* Math operators *)
  | '+'                 { M_PLUS }
  | '-'                 { M_MINUS }
  | '*'                 { M_TIMES }
  | '/'                 { M_DIV }
  | '%'                 { M_MOD }

  (* Other operators *)
  | '='                 { O_ASSIGN }
  | '&'                 { O_REF }

  (* Symbols *)
  | '('                 { L_PAREN }
  | ')'                 { R_PAREN }

  | '{'                 { L_BRACKET }
  | '}'                 { R_BRACKET }

  | '['                 { L_SQUARE }
  | ']'                 { R_SQUARE }

  | '.'                 { DOT }
  | ','                 { COMMA }
  | ':'                 { COLON }
  | ';'                 { SEMICOLON }
  | "<-"                { LINK }

  (* Comments *)
  | "//"                { comments lexbuf }
  | "/*"                { comments lexbuf }

  | eof                 { EOF }

  | _                   { failwith "Not implemented yet" }

and comments = parse
  | _                   { comment_single_line lexbuf }
  | "*/"
  | ['\n']              { next_token lexbuf }

{
  let _ = 
    let lexbuf = Lexing.from_channel stdin in 
    next_token lexbuf
}
