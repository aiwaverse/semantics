(** Tipo de uma expressão em L0 *)
type expr =
  | True
  | False
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr
  | If of expr * expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr

type lZeroType = TNat | TBool

let rec typeinfer e =
  match e with
  | True -> Some TBool
  | False -> Some TBool
  | Zero -> Some TNat
  | Succ ee when typeinfer ee = Some TNat -> Some TNat
  | Pred ee when typeinfer ee = Some TNat -> Some TNat
  | IsZero ee when typeinfer ee = Some TNat -> Some TBool
  | If (cond, e_true, e_false)
    when typeinfer cond = Some TBool && typeinfer e_false = typeinfer e_true ->
      typeinfer e_true
  | And (e1, e2) when typeinfer e1 = Some TBool && typeinfer e2 = Some TBool ->
      Some TBool
  | Or (e1, e2) when typeinfer e1 = Some TBool && typeinfer e2 = Some TBool ->
      Some TBool
  | Not ee when typeinfer ee = Some TBool -> Some TBool
  | _ -> None

(** Função auxiliar para imprimir uma árvore qualquer*)
let rec print_expr e =
  match e with
  | True -> "True"
  | False -> "False"
  | Zero -> "0"
  | Succ ee -> "(Succ " ^ print_expr ee ^ ")"
  | Pred ee -> "(Pred " ^ print_expr ee ^ ")"
  | IsZero ee -> "(IsZero " ^ print_expr ee ^ ")"
  | If (e1, e2, e3) ->
      "(If (" ^ print_expr e1 ^ ") then (" ^ print_expr e2 ^ ") else ("
      ^ print_expr e3 ^ "))"
  | And (e1, e2) -> "(And " ^ print_expr e1 ^ ", " ^ print_expr e2 ^ ")"
  | Or (e1, e2) -> "(Or " ^ print_expr e1 ^ ", " ^ print_expr e2 ^ ")"
  | Not e1 -> "(Not " ^ print_expr e1 ^ ")"

(** Verifica se um valor é numérico sem avaliar o termo *)
let rec is_numerical e =
  match e with Zero -> true | Succ nv -> is_numerical nv | _ -> false

(** Função que dá um passo na avaliação*)
let rec step e =
  match e with
  | True -> None
  | False -> None
  | Zero -> None
  | Succ _ -> apply_succ e
  | Pred _ -> apply_pred e
  | IsZero _ -> apply_is_zero e
  | If _ -> apply_if e
  | And _ -> apply_and e
  | Or _ -> apply_or e
  | Not _ -> apply_not e

and apply_succ e =
  match e with
  | Succ ee -> (
      match step ee with None -> None | Some stepped -> Some (Succ stepped))
  | _ -> None

and apply_pred e =
  match e with
  | Pred nv -> (
      if is_numerical nv then
        match nv with Zero -> Some Zero | Succ e -> Some e | _ -> None
      else
        match step nv with None -> None | Some stepped -> Some (Pred stepped))
  | _ -> None

and apply_is_zero e =
  match e with
  | IsZero nv -> (
      if is_numerical nv then
        match nv with Zero -> Some True | Succ _ -> Some False | _ -> None
      else
        match step nv with
        | None -> None
        | Some stepped -> Some (IsZero stepped))
  | _ -> None

and apply_if e =
  match e with
  | If (True, opt_true, _) -> Some opt_true
  | If (False, _, opt_false) -> Some opt_false
  | If (cond, opt_true, opt_false) -> (
      match step cond with
      | None -> None
      | Some stepped -> Some (If (stepped, opt_true, opt_false)))
  | _ -> None

and apply_and e =
  match e with
  | And (True, True) -> Some True
  | And (True, False) -> Some False
  | And (False, True) -> Some False
  | And (False, False) -> Some False
  | And (e1, e2) -> (
      match step e1 with
      | Some stepped_e1 -> Some (And (stepped_e1, e2))
      | None -> (
          match step e2 with
          | Some stepped_e2 -> Some (And (e1, stepped_e2))
          | None -> None))
  | _ -> None

and apply_or e =
  match e with
  | Or (True, True) -> Some True
  | Or (True, False) -> Some True
  | Or (False, True) -> Some True
  | Or (False, False) -> Some False
  | Or (e1, e2) -> (
      match step e1 with
      | Some stepped_e1 -> Some (Or (stepped_e1, e2))
      | None -> (
          match step e2 with
          | Some stepped_e2 -> Some (Or (e1, stepped_e2))
          | None -> None))
  | _ -> None

and apply_not e =
  match e with
  | Not True -> Some False
  | Not False -> Some True
  | Not e1 -> (
      match step e1 with
      | Some stepped_e1 -> Some (Not stepped_e1)
      | None -> None)
  | _ -> None

(** eval aplica step e retorna uma lista de passos até o final*)
let rec eval e =
  let proper_eval e' =
    match step e' with
    | None -> [ print_expr e' ]
    | Some stepped -> print_expr e' :: eval stepped
  in
  match typeinfer e with
  | None -> [ print_expr e; "Expressão mal tipada" ]
  | Some _ -> proper_eval e

(** result usa eval e retorna o último elemento da lista (o resultado) *)
let result e =
  let rec last lst =
    match lst with
    | [] -> None
    | [ result ] -> Some result
    | _ :: xxs -> last xxs
  in
  Option.value (last (eval e)) ~default:""
