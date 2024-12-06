{
open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
let var = ['a'-'z']+
let num = '-'? ['0'-'9']+

rule read =
  parse
  | ":" { COLON }
  | "int" { INTTY }
  | "bool" { BOOLTY }
  | "fun" { FUN }
  | "let" { LET }
  | "rec" { REC }
  | "=" { EQ }
  | "in" { IN }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "true" { TRUE }
  | "false" { FALSE }
  | "assert" { ASSERT }
  | "()" { UNIT }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
