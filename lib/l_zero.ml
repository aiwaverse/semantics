open Either
open List
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
  | True -> Left "Can't step True"
  | False -> Left "Can't step False"
  | Zero -> Left "Can't step Zero"
  | Succ _ -> apply_succ e
  | Pred _ -> apply_pred e
  | IsZero _ -> apply_is_zero e
  | If (_, _, _) -> apply_if e

and apply_if e =
  match e with
  | If (True, e1, _) -> Right e1
  | If (False, _, e2) -> Right e2
  | If (cond, e1, e2) -> (
      match step cond with
      | Left err -> Left ("Can't apply If, internal step failed with: " ^ err)
      | Right ee -> Right (If (ee, e1, e2)))
  | _ -> Left ("Can't apply If on " ^ print_expr e)

and apply_is_zero e =
  match e with
  | IsZero Zero -> Right True
  | IsZero (Succ _) -> Right False
  | IsZero ee -> (
      match step ee with
      | Left err ->
          Left ("Can't apply IsZero, internal step failed with: " ^ err)
      | Right eee -> Right (IsZero eee))
  | _ -> Left ("Can't apply IsZero on " ^ print_expr e)

and apply_pred e =
  match e with
  | Pred Zero -> Right Zero
  | Pred (Succ ee) ->
      if is_numerical ee then Right ee
      else Left ("Can't apply Pred, value is not numerical: " ^ print_expr ee)
  | Pred ee -> (
      match step ee with
      | Left err -> Left ("Can't apply Pred, internal step failed with: " ^ err)
      | Right eee -> Right (Pred eee))
  | _ -> Left ("Can't apply Pred on" ^ print_expr e)

and apply_succ e =
  match e with
  | Succ ee -> (
      match step ee with
      | Left err -> Left ("Can't apply Succ, internal step failed with: " ^ err)
      | Right eee -> Right (Succ eee))
  | _ -> Left ("Can't apply Pred on" ^ print_expr e)

let rec eval e =
  match step e with
  | Left stop -> [ print_expr e; "Evaluation stopped at " ^ stop ]
  | Right ee -> print_expr e :: eval ee

let result e =
  let rec second_to_last lst =
    match lst with
    | [] -> None
    | [ res; _ ] -> Some res
    | _ :: xxs -> second_to_last xxs
  in
  second_to_last (eval e)
