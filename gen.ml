(* Compilation functions *)

open Lang
open Analyses
open Instrs

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)
let rec position = function
      (e,x::l) -> if x = e then 1 else 1 + position(e,l)
    | _ -> failwith "Element not found!"
;;

let rec gen_expr varList = function
    Const (tp, c) -> [Loadc (tp,c)]
  | VarE (_, Var (_, var)) -> [Loadv (IntT, position(var,varList))]
  | BinOp (tp, op, e1, e2) -> let gen_expr_better_than_ever = (gen_expr varList) in
                                (gen_expr_better_than_ever e1) @ (gen_expr_better_than_ever e2) @ [Bininst (tp,op)]
  | _ -> failwith "not implemented yet!"
;;
