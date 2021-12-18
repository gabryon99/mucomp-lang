exception Syntax_error of Location.lexeme_pos * string

let parse scanner input = 
  Parser.compilation_unit scanner input