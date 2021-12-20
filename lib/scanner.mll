{
  (* OCaml declaration*)   
  open Parser
  exception Lexing_error of Location.lexeme_pos * string    

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keywords_table = create_hashtable 18 [
    ("true",      BOOL(true));
    ("false",     BOOL(false));
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
  let check_identifier id lexbuf = 
    if String.length id < max_id_length then 
      ID(id) 
    else 
      let msg = Printf.sprintf "The identifier \"%s\" must be < %d characters!\n" id max_id_length in
      raise (Lexing_error (Location.to_lexeme_position lexbuf, msg))
  
  let check_num_int32 num lexbuf =
    if num > max_int || num < min_int then
      let msg = Printf.sprintf "The number \"%d\" is not a 32 bit integer!\n" num in 
      raise (Lexing_error (Location.to_lexeme_position lexbuf, msg))
    else
      INT(num)
  
}

(* Declaration of regular expressions *)

let digit = ['0' - '9']
let dec_number = digit+
let hex_number = "0x"['0' - '9' 'a' - 'f' 'A' - 'F']+

let identifier = ['a' - 'z' 'A' - 'Z']['0' - '9' 'a' - 'z' 'A' - 'Z' '_']*

let blank = [' ' '\t']

(* Declaration of scanner functions *)

rule next_token = parse

  | blank       { next_token lexbuf }
  
  | ['\n'] 
  | "\r\n"     { Lexing.new_line lexbuf; next_token lexbuf}

  (* Comments *)
  | "//"                { let _ = single_line_comment lexbuf in next_token lexbuf }
  | "/*"                { let _ = multi_line_comment lexbuf in next_token lexbuf }

  (* Primitives *)
  | hex_number
  | dec_number as num   { 
                          let n = int_of_string num in 
                          check_num_int32 n lexbuf
                        }

  | identifier as id    { 
                          try
                            let token = Hashtbl.find keywords_table id in
                            token
                          with Not_found ->
                            check_identifier id lexbuf
                        }
  | [''']               { character ' ' lexbuf }
  
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

  | "++"                { M_PLUS_PLUS }
  | "--"                { M_PLUS_PLUS }

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

  | eof                 { EOF }

  | _ as c              { Printf.printf "Unmatched: %c\n" c; raise (Lexing_error((Location.to_lexeme_position lexbuf), "Unrecognized token!")) }

and single_line_comment = parse
  | "\r\n"
  | "\n"                { Lexing.new_line lexbuf; () }
  | _                   { single_line_comment lexbuf }
and multi_line_comment = parse
  | "*/"                { () }
  | "\n"
  | "\r\n"              { Lexing.new_line lexbuf; multi_line_comment lexbuf }
  | _                   { multi_line_comment lexbuf }
and character c = parse
  | [''']               { CHAR(c) }
  | ['\\']['n']         { character '\n' lexbuf}
  | ['\\']['t']         { character '\t' lexbuf}
  | ['\\']['r']         { character '\r' lexbuf}
  | [^'''] as c         { character c lexbuf }
  | _                   { raise (Lexing_error((Location.to_lexeme_position lexbuf), "Unrecognized character!"))}

{}
