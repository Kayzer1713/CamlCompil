(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     globalvar: (vname * tp) list;
     returntp: tp;
     funbind: fundecl list}

let tp_of_vardecl (Vardecl (t, _)) = t;;
let tp_of_expr = function
   Const (t, _) -> t
 | VarE (t, _) -> t
 | BinOp (t, _, _, _) -> t
 | IfThenElse (t, _, _, _) -> t
 | CallE (t, _, _) -> t;;

(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;

exception TypeNotMatching;;
exception UndefinedFunction;;
exception TooManyArguments;;
exception MissingArguments;;

(*Take a value and return the type of this value*)
let auxConst = function
    BoolV c -> BoolT
  | VoidV -> raise TypeNotMatching
  | IntV c -> IntT
;;

(*Take a var name and return the type of this var in the current env
Raise an exception is the var doesn't exist in the current env*)
let rec auxVar = function
    (name, (nomV, tpV)::listVar) -> if name = nomV then tpV else auxVar(name, listVar)
  | _ -> raise TypeNotMatching
;;

(*Take a binoop with two typed expr and return the type of the binop typed*)
let binopAux = function
    (BArith _, e1, e2) -> let tp1 = tp_of_expr(e1) and tp2 = tp_of_expr(e2) in if tp1 = IntT && tp2 = IntT then IntT else raise TypeNotMatching
  | (BCompar op, e1, e2) -> let tp1 = tp_of_expr(e1) and tp2 = tp_of_expr(e2) in if tp1 = tp2 then BoolT else raise TypeNotMatching
  | (BLogic op, e1, e2) -> let tp1 = tp_of_expr(e1) and tp2 = tp_of_expr(e2) in if tp1 = BoolT && tp2 = BoolT then BoolT else raise TypeNotMatching
;;

(*Take a fonction name and the funbind of the current env and return a couple
composed by the tp and the vardecl list of this function in the current env*)
let rec auxCallE = function
    (funName, (Fundecl (tp, fname, listArg))::listFun) -> if funName = fname then (tp,listArg) else auxCallE(funName, listFun)
  | _ -> raise UndefinedFunction
;;

let rec tp_expr env = function
    Const (_, c) -> Const ((auxConst c),c)
  | VarE (_, Var (bind, name)) -> if bind = Local then VarE ((auxVar(name,env.localvar)), Var (bind, name)) else VarE ((auxVar(name,env.globalvar)), Var (bind, name))
  | BinOp (_, op, e1, e2) -> let e1 = ((tp_expr env) e1) and e2 = ((tp_expr env) e2) in BinOp (((binopAux(op, e1, e2))), op, e1, e2)
  | IfThenElse (_, cond, e1, e2) -> let tpCond = (tp_expr env cond) and tp1 = (tp_expr env e1) and tp2 = (tp_expr env e2) in
                                      if (tp_of_expr tpCond) = BoolT && (tp_of_expr tp1) = (tp_of_expr tp2)
                                        then IfThenElse((tp_of_expr(tp1)), tpCond, tp1, tp2)
                                        else raise TypeNotMatching
  | CallE (_, fName, givenArgs) -> let (fType, argList) = auxCallE(fName, env.funbind) in
                                                let rec argTest = function
                                                    ((Vardecl (varTp, _))::vardeclList, arg::givenArgList) -> let typedArg = (tp_expr env arg) in
                                                                                    if varTp = tp_of_expr(typedArg)
                                                                                      then typedArg::argTest(vardeclList, givenArgList)
                                                                                      else raise TypeNotMatching
                                                  | ([],[]) -> []
                                                  | (_,[]) -> raise MissingArguments
                                                  | ([],_) -> raise TooManyArguments
                                                in  CallE(fType, fName, argTest(argList, givenArgs))
;;
