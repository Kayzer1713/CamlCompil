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

(*main function that take an expression and return the corresponding instructions
take for args one expression (the one to translate in an instruction list, the var list and the current path (for the labels)*)
let rec gen_expr varList path = function
    Const (tp, c) -> [Loadc (tp,c)]
  | VarE (_, Var (_, var)) -> [Loadv (IntT, position(var,varList))]
  | BinOp (tp, op, e1, e2) -> let gen_expr_better_than_ever = (gen_expr varList path) in
    (gen_expr_better_than_ever e1) @ (gen_expr_better_than_ever e2) @ [Bininst (tp,op)]
  | IfThenElse (tp, op, e1, e2) -> let vFalse = path @ [0] and vEnd = path @ [2] in
    (gen_expr varList vEnd op) (*concaténation des instructions*)
    @ [Loadc (IntT, (IntV 0))]
    @ [If (BCeq, vFalse)]
    @ (gen_expr varList (path @ [1]) e1)
    @ [Goto vEnd]
    @ [Label vFalse]
    @ (gen_expr varList vFalse e2)
    @ [Label vEnd]
  | CallE (tp, fname, givenArgs) ->
  let rec genCallE instr_list type_list = function
    | [] -> (instr_list, type_list)
    | (e::l) -> let instrElt = (gen_expr varList path e) and tpElt = (tp_of_expr e) in
      genCallE (instr_list @ instrElt) (type_list @ [tpElt]) l
  in let (instr_list, type_list) = (genCallE [] [] givenArgs) in
  instr_list @ [Invoke (tp, fname, type_list)] @ [ReturnI tp]
;;

(*Compilation funcion*)
let gen_prog (Prog (gvds, fdfs)) =
  JVMProg ([],
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])
  ;;
