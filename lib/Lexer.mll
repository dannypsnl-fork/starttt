{
  open Parser
}

let digit = ['0'-'9']
let sign = ['-' '+']
let exponent = ['e' 'E']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '-')*
let string_literal = '"'  '"'

let whitespace = [' ' '\t']+

let digit = ['0'-'9']
let sign = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+

let exponent = ['e' 'E']
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '-')*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

(* Rules *)

rule token = parse
  | "let" { LET }
  | "λ" { LAM }
  | "lam" { LAM }
  | '.' { DOT }
  | '=' { EQ }
  | ':' { COLON }
  | ';' { COMMA }
  | '{' { L_BRACE }
  | '}' { R_BRACE }
  (* type. *)
  | '@' { TYPE_MARK }
  | "->" { ARROW }
  (* term. *)
  | '#' { CONST_MARK }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  (* etc. *)
  | whitespace { token lexbuf }
  | newline  { token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
