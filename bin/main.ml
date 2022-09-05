open Lexing
module L = Starttt.Lexer
module P = Starttt.Parser
module C = Starttt.Check
module Ty = Starttt.Type
module Ast = Starttt.Ast

let rec parse' f source =
  let lexbuf = Lexing.from_channel source in
  try f L.token lexbuf
  with P.Error ->
    raise (Failure ("Parse error at " ^ pos_string lexbuf.lex_curr_p))

and pos_string pos =
  let l = string_of_int pos.pos_lnum and c = string_of_int (column pos + 1) in
  "line " ^ l ^ ", column " ^ c

and column pos = pos.pos_cnum - pos.pos_bol - 1

let parse_program source = parse' P.program source

let () =
  Printexc.register_printer (function
    | Ast.TypeMismatch (t1, t2) ->
        Some
          (Printf.sprintf "`%s` not equals to `%s`" (Ty.type_to_str t1)
             (Ty.type_to_str t2))
    | _ -> None);

  print_string "starttt";
  print_newline ();
  let file_name = Array.get Sys.argv 1 in
  (* read file *)
  let in_chan = open_in file_name in
  (* parsing *)
  let program = parse_program in_chan in
  (* check type *)
  C.check_all C.empty_context program;
  (* evaluate the program *)
  ()
