open Option

type expr =
  | True
  | False
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr
  | If of expr * expr * expr

let rec print_expr e =
  match e with
  | True -> "True"
  | False -> "False"
  | Zero -> "0"
  | Succ ee -> "(Succ " ^ print_expr ee ^ ")"
  | Pred ee -> "(Pred " ^ print_expr ee ^ ")"
  | IsZero ee -> "(IsZero " ^ print_expr ee ^ ")"
  | If (e1, e2, e3) ->
      "(If (" ^ print_expr e1 ^ ", " ^ print_expr e2 ^ ", " ^ print_expr e3
      ^ "))"

let rec is_numerical e =
  match e with Zero -> true | Succ nv -> is_numerical nv | _ -> false

let rec step e =
  match e with
  | True -> None
  | False -> None
  | Zero -> None
  | Succ _ -> apply_succ e
  | Pred _ -> apply_pred e
  | IsZero _ -> apply_is_zero e
  | If _ -> apply_if e

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

let rec eval e =
  match step e with
  | None -> [ print_expr e ]
  | Some stepped -> print_expr e :: eval stepped

let result e =
  let rec last lst =
    match lst with
    | [] -> None
    | [ result ] -> Some result
    | _ :: xxs -> last xxs
  in
  Option.value (last (eval e)) ~default:""
