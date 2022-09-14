(* This module should return a really to run code!

   conversion of module:
   1. rearrange the definitions
      extracts the definition with non-mono type(here, only arrow type) to the front of the program and binds them with their type
   2. check term of definition with mono type
   3. check term of definition with non-mono type now, because all type bindings are ready now for mutual access

   when converting a module, remember no redefined allowed here
*)
open Type
open Ast
open Check

let isMono : top -> bool =
 fun t ->
  match t with Let (_, Arrow (_, _), _) -> false | Let (_, _, _) -> true

let conversion : program -> program =
 fun p ->
  let init_ctx = empty_context in
  let monos, non_monos = List.partition isMono p in
  check_all init_ctx non_monos;
  check_all init_ctx monos;
  List.append non_monos monos
