(* Analyses of source language statements:
 - predicate: 'statement returns'
 - depth of operand stack for expression evaluation
 - definite assignment
*)

open Lang

(* ************************************************************ *)
(* ****  Statement returns                                 **** *)
(* ************************************************************ *)

let rec stmt_returns = false



(* ************************************************************ *)
(* ****  Stack depth                                       **** *)
(* ************************************************************ *)



let rec stack_depth_e = 0

let rec stack_depth_c = 0



(* ************************************************************ *)
(* ****  Definite Assignment                               **** *)
(* ************************************************************ *)

module StringSet =
  Set.Make
    (struct type t = string
	    let compare = Pervasives.compare
     end)

let rec defassign_e a = true

let rec defassign_c allvs a = a

(*calc how many element there is in a list*)
let rec listsum = function
	| [] -> 0
	| (e::l) -> e + (listsum l)
;;

(*function to calc the higth of the operand stack*)
let rec stack_depth_e = function
    Const (_, _) -> 1
  | VarE (vType, Var (varBinding, vVname)) -> 1
	| BinOp (_, _, bExpr1, bExpr2) -> max (stack_depth_e bExpr1) (stack_depth_e bExpr2)
	| IfThenElse (_, cond, e1, e2) -> (stack_depth_e cond) + (max (stack_depth_e e1) (stack_depth_e e2))
	| CallE (_, _, args) -> listsum (List.map stack_depth_e args)
;;

(*function to calc the higth of the operand stack*)
let rec stack_depth_c = function
	  Skip -> 0
	| Assign (_, _, e) -> stack_depth_e e
	| Seq (s1, s2) -> (stack_depth_c s1) + (stack_depth_c s2)
	| Cond (cond, c1, c2) -> (stack_depth_e cond) + (max (stack_depth_c c1) (stack_depth_c c2))
	| While (cond, whileStmt) -> (stack_depth_e cond) + (stack_depth_c whileStmt)
  | CallC (_, e) -> listsum (List.map stack_depth_e e)
	| Return returnExpr -> stack_depth_e returnExpr
;;
