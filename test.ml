(*load all modules*)
#use "gen.ml";;
#use "lang.ml";;
#use "instrs.ml";;
#use "typing.ml";;
#use "analyses.ml";;
#use "use.ml";;
open Lang;;
open Instrs;;
open Analyses;;
open Print_instr;;
open Interf;;

let env = {localvar = [("k", IntT); ("n", IntT)];
          globalvar = [];
          returntp = VoidT;
          funbind = [Fundecl(IntT , "f", [Vardecl(IntT , "n"); Vardecl(BoolT , "b")])]}
;;

let x = BinOp (BoolT ,BCompar BCeq , VarE (IntT , Var (Local , "n")),
        BinOp (IntT , BArith BAadd , VarE (IntT , Var (Local , "k")),
        Const (IntT , IntV 1)));;

let typed_expr = tp_expr env x;;
gen_expr ["n";"k"] typed_expr;;
