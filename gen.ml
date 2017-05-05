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
    let rec exprToInstrList instr_list type_list = function
      | [] -> (instr_list, type_list)
      | (e::l) -> let instrElt = (gen_expr varList path e) and tpElt = (tp_of_expr e) in
          exprToInstrList (instr_list @ instrElt) (type_list @ [tpElt]) l in
            let (instr_list, type_list) = (exprToInstrList [] [] givenArgs) in
              instr_list
              @ [Invoke (tp, fname, type_list)]
              @ [ReturnI tp]
;;

(*Same as gen_expr but for stmt instead of epr*)
let rec gen_stmt varList path = function
    Skip -> [Nop]
  | Assign (tp, Var (tpVar, nVar), e) -> (gen_expr varList path e) @ [Storev (tp, position(nVar, varList))]
	| Seq (s1, s2) -> (gen_stmt varList path s1) @ (gen_stmt varList path s2)
	| Cond (c, c1, c2) -> let vFalse = path @ [0] and vEnd = path @ [2] in
		(gen_expr varList vEnd c)
    @ [Loadc (IntT, (IntV 0))]
    @ [If (BCeq, vFalse)]
    @ (gen_stmt varList (path @ [1]) c1)
    @ [Goto vEnd]
    @ [Label vFalse]
    @ (gen_stmt varList vFalse c2)
    @ [Label vEnd]
	| While (e, stmt) -> let vFalse = path @ [0] and whileStmt = path @ [1] in
		[Label whileStmt]
    @ (gen_expr varList (path @ [2]) e)
    @ [Loadc (IntT, (IntV 0))]
    @ [If (BCeq, vFalse)]
    @ (gen_stmt varList whileStmt stmt)
    @ [Goto whileStmt]
    @ [Label vFalse] @ [Nop]
  | CallC (c, stmt) ->
    let rec stmtToInstrList instr_list type_list = function
      | [] -> (instr_list, type_list)
      | (e::l) -> let instrElt = (gen_expr varList path e) and tpElt = (tp_of_expr e) in
          stmtToInstrList (instr_list @ instrElt) (type_list @ [tpElt]) l in
            let (instr_list, type_list) = (stmtToInstrList [] [] stmt) in
              instr_list
              @ [Invoke (VoidT, c, type_list)]
              @ [ReturnI VoidT]
	| Return returnExpr -> [ReturnI (tp_of_expr returnExpr)]
;;

(*Function that take a Vardecl list and return in the rigth order a list of type of those var*)
let rec varTypeList = function
    [] -> []
  | (Vardecl (tpVar, _)::l) -> tpVar::(varTypeList l)
;;

(*Function that take a liste of Vardecl and return in the rigth order the list of name of those var*)
let rec varNameList = function
    [] -> []
  | (Vardecl (_, varName)::l) -> varName::(varNameList l)
;;

(*function that translate a fun declaration*)
let gen_fundefn = function Fundefn (Fundecl (fType, fName, fArgs), varList, funStmt) ->
	Methdefn (
    Methdecl (fType, fName, (varTypeList fArgs)),
    Methinfo (2, 1),
    (gen_stmt ((varNameList varList) @ (varNameList fArgs)) [] funStmt)
  )
;;

(*Compilation funcion*)
let gen_prog = function Prog (globalVars, funList) -> JVMProg ([], List.map gen_fundefn funList);;
