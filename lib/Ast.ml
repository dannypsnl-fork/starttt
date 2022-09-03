exception TODO

module Map = Hashtbl

type typ = TyInt | TyFloat | TyString

exception TypeMismatch of typ * typ

type term =
  | Int of int
  | Float of float
  | String of string
  | Lambda of string * term

type top = Let of string * typ * term
type program = top list

(* global context bounds
   ---------------------
      string => type *)
type context = (string, typ) Map.t

let empty_context : context = Map.create 0

let rec check_all : context -> program -> unit =
 fun ctx p ->
  match p with
  | t :: rest ->
      check_top ctx t;
      check_all ctx rest
  | [] -> ()

and check_top : context -> top -> unit =
 fun ctx tm ->
  match tm with
  | Let (x, ty, e) ->
      let ty' = infer_ty e in
      equate_ty ty ty';
      Map.add ctx x ty

and infer_ty : term -> typ =
 fun t ->
  match t with
  | Int _ -> TyInt
  | Float _ -> TyFloat
  | String _ -> TyString
  | Lambda (_, _) -> raise TODO

and equate_ty : typ -> typ -> unit =
 fun t1 t2 ->
  match (t1, t2) with
  | TyInt, TyInt -> ()
  | TyFloat, TyFloat -> ()
  | TyString, TyString -> ()
  | _ -> raise (TypeMismatch (t1, t2))
