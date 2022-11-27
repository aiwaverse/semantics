open Semantics.L_zero

let () = print_endline (print_expr (If (IsZero (Succ Zero), False, False)))
