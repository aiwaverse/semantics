type expr =
  | ExTrue
  | ExFalse
  | ExIf of expr * expr * expr
  | ExZero
  | ExSucc of expr
  | ExPred of expr
  | ExIsZero of expr
  | ExAnd of expr * expr
  | ExOr of expr * expr
  | ExNot of expr

(* [isnumerival nv] é verdadeiro se [e] é valor numérico. É falso caso contrário  *)
let rec isnumericval (e : expr) : bool =
  match e with ExZero -> true | ExSucc e1 -> isnumericval e1 | _ -> false

(* [isvalue e] é verdadeiro se [e] é valor de L0. É falso caso contrário *)
let isvalue (e : expr) : bool = isnumericval e || e = ExTrue || e = ExFalse

(* Excecao ativada quando houver tentativa de dar um passo com FORMA NORMAL *)
exception NoRuleApplies

(* [step e] é resultado da avaliação em um passo de [e]
   Ativa exceção [NoRuleApplies] se [e]  é forma normal *)
let rec step (e : expr) : expr =
  match e with
  (* CASO IF(e1,e2,e3)   *)
  | ExIf (ExTrue, e2, _) -> e2 (* regra E-IfTrue *)
  | ExIf (ExFalse, _, e3) -> e3 (* regra E-False *)
  | ExIf (e1, e2, e3) ->
      (* regra E-If *)
      let e1' = step e1 in
      ExIf (e1', e2, e3)
  (* CASO SUCC e1   *)
  | ExSucc e1 ->
      (* regra E-Succ *)
      let e1' = step e1 in
      ExSucc e1'
  (* CASO  PRED e1   *)
  | ExPred ExZero -> ExZero (* regra E-PredZero *)
  | ExPred (ExSucc nv) when isnumericval nv -> nv (* regra E-PredSucc *)
  | ExPred e1 ->
      (* regra E-Pred *)
      let e1' = step e1 in
      ExPred e1'
  (* CASO ISZERO(e1)   *)
  | ExIsZero ExZero -> ExTrue (* regra E$-$IsZeroZero *)
  | ExIsZero (ExSucc nv) when isnumericval nv ->
      ExFalse (* regra E-IsZeroSucc *)
  | ExIsZero e1 ->
      (* regra E-IsZero *)
      let e1' = step e1 in
      ExIsZero e1'
  (* CASO AND(e1,e2) *)
  | ExAnd (ExTrue, ExTrue) -> ExTrue
  | ExAnd (ExTrue, ExFalse) -> ExFalse
  | ExAnd (ExFalse, ExTrue) -> ExFalse
  | ExAnd (ExFalse, ExFalse) -> ExFalse
  | ExAnd (v1, e2) when isvalue v1 ->
      let e2' = step e2 in
      ExAnd (v1, e2')
  | ExAnd (e1, e2) ->
      let e1' = step e1 in
      ExAnd (e1', e2)
  (* CASO OR(e1,e2) *)
  | ExOr (ExTrue, ExTrue) -> ExTrue
  | ExOr (ExTrue, ExFalse) -> ExTrue
  | ExOr (ExFalse, ExTrue) -> ExTrue
  | ExOr (ExFalse, ExFalse) -> ExFalse
  | ExOr (v1, e2) when isvalue v1 ->
      let e2' = step e2 in
      ExOr (v1, e2')
  | ExOr (e1, e2) ->
      let e1' = step e1 in
      ExOr (e1', e2)
  (* CASO NOT e *)
  | ExNot ExTrue -> ExFalse
  | ExNot ExFalse -> ExTrue
  | ExNot e1 ->
      let e1' = step e1 in
      ExNot e1'
  (* CASO Nenhuma regra se aplique  *)
  | _ -> raise NoRuleApplies

(* [eval e] é a forma normal de [e]  *)
let rec eval (e : expr) : expr =
  try
    let e' = step e in
    eval e'
  with NoRuleApplies -> e

(* == Funções auxiliares para experimentos com exemplos  ===== *)

(* [string_of_expr e] é o string que representa [e] *)
let rec string_of_expr (e : expr) : string =
  match e with
  | ExTrue -> "true"
  | ExFalse -> "false"
  | ExIf (e1, e2, e3) ->
      " if " ^ string_of_expr e1 ^ " then " ^ string_of_expr e2 ^ " else "
      ^ string_of_expr e3
  | ExZero -> "0"
  | ExSucc e1 -> " succ (" ^ string_of_expr e1 ^ ")"
  | ExPred e1 -> " pred (" ^ string_of_expr e1 ^ ")"
  | ExIsZero e1 -> " iszero (" ^ string_of_expr e1 ^ ")"
  | ExAnd (e1, e2) ->
      " and (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | ExOr (e1, e2) ->
      " or (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | ExNot e1 -> " not (" ^ string_of_expr e1 ^ ")"

let print_eval (e : expr) : unit =
  let e' = eval e in
  print_string
    ((if isvalue e' then "termina com valor " else "termina com erro ")
    ^ string_of_expr e')

let print_steps e =
  let rec prt_steps (e : expr) : unit =
    try
      let e' = step e in
      print_endline ("---> " ^ string_of_expr e');
      prt_steps e'
    with NoRuleApplies ->
      print_endline ("-/-> " ^ if isvalue e then "/* valor" else "/* erro")
  in
  print_endline ("     " ^ string_of_expr e);
  prt_steps e

(* if  iszero ( 0 ) then  pred ( 0 ) else  0 *)
let ex1 = ExIsZero ExZero
let ex2 = ExPred ExZero
let ex3 = ExZero
let ex = ExIf (ex1, ex2, ex3)
let ex4 = ExIf (ExZero, ExPred ExZero, ExSucc (ExSucc ExZero))

(*  pred (succ (iszero 0))   *)
let ex5 = ExPred (ExSucc (ExIsZero ExZero))

(* if  pred (pred (succ (succ 0))) then  0  else  succ 0 *)
let ex61 = ExPred (ExPred (ExSucc (ExSucc ExZero)))
let ex6 = ExIf (ex61, ExZero, ExSucc ExZero)
