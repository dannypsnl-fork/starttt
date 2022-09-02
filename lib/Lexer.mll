{
  (* open Lexing *)
  open Parser

  let make_table num elems =
    let table = Hashtbl.create num in
    List.iter (fun (k, v) -> Hashtbl.add table k v) elems;
    table

  let keywords =
    make_table 0 [
      ("let", LET);
      ("lam", LAM);
      ("λ", LAM);
      ("=", EQ);
    ]
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
  | int_constant { INT_CONSTANT (int_of_string (Lexing.lexeme lexbuf)) }
  | float_constant { FLOAT_CONSTANT (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier { WORD (Lexing.lexeme lexbuf) }
  (* etc. *)
  | whitespace { token lexbuf }
  | eof { EOF }
  | _ { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
