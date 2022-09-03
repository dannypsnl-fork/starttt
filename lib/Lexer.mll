{
  (* open Lexing *)
  open Parser
}

let digit = ['0'-'9']
let sign = ['-' '+']
let exponent = ['e' 'E']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '-')*

let whitespace = [' ' '\t']+

let digit = ['0'-'9']
let sign = ['-' '+']
let alpha = ['a'-'z' 'A'-'Z']

let int_constant = sign? digit+

let exponent = ['e' 'E']
let float_constant = sign? digit+ '.' digit+ (exponent sign? digit+)?
let identifier = alpha (alpha | digit | '-')*

let whitespace = [' ' '\t']+

(* Rules *)

rule token = parse
  | "let" { LET }
  | "λ" { LAM }
  | "lam" { LAM }
  | '=' { EQ }
  | "Int" { INT }
  | "Float" { FLOAT }
  | "String" { STRING }
  | int_constant { INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_constant { FLOAT_CONSTANT (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier { STRING_LITERAL (Lexing.lexeme lexbuf) }
  (* etc. *)
  | whitespace { token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
