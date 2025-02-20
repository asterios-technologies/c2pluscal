open Pc

let print_pc_var out ((n): pc_var) =
  Format.fprintf out "VAR : name %s ;" n

let print_pc_binop out (b: pc_binop) = match b with
  PAdd -> Format.fprintf out "PAdd";
  | PSub -> Format.fprintf out "PSub";
  | PMul -> Format.fprintf out "PMul";
  | PDiv -> Format.fprintf out "PDiv";
  | PMod -> Format.fprintf out "PMod";
  | PAddPI -> Format.fprintf out "PAddPI";
  | PSubPI -> Format.fprintf out "PSubPI";
  | PSubPP -> Format.fprintf out "PSubPP";
  | PLt -> Format.fprintf out "PLt";
  | PGt -> Format.fprintf out "PGt";
  | PLe -> Format.fprintf out "PLe";
  | PGe -> Format.fprintf out "PGe";
  | PEq -> Format.fprintf out "PEq";
  | PNe -> Format.fprintf out "PNe";
  | PLand -> Format.fprintf out "PLand";
  | PLor -> Format.fprintf out "PLor"

let print_pc_unop out (u: pc_unop) = match u with
  |PMinus -> Format.fprintf out "PMinus";
  |PNot -> Format.fprintf out "PNot"

let rec print_pc_cst out (c: pc_cst) = match c with
  |PInt(i) -> Format.fprintf out "PInt(%s)" (string_of_int i);
  |PString(s) -> Format.fprintf out "PString(%s)" s
  |PRecord(l) -> Format.fprintf out "PRecord(";
                 List.iter (fun (s,pc_exp) ->
                  Format.fprintf out "%s : " s;
                  print_pc_expr out pc_exp;
                  Format.fprintf out ",") l;
                Format.fprintf out ")";
  |PArray(l) -> Format.fprintf out "PArray(";
                List.iter (fun (_,pc_exp) ->
                 print_pc_expr out pc_exp;
                 Format.fprintf out ",") l;
               Format.fprintf out ")";
  |PEnumItem(item_name) -> Format.fprintf out "PEnumItem(%s)" item_name

and print_pc_lval out (lval: pc_lval) = match lval with
PLVar(ptr,_) -> Format.fprintf out "%s" ptr;
(* |PLPtr(ptr,_) -> Format.fprintf out "%s" ptr; *)
|PLoad(lval_prime) -> Format.fprintf out "load(";print_pc_lval out lval_prime;Format.fprintf out ")";
|PField(field,lval_prime) -> print_pc_lval out lval_prime; Format.fprintf out ".%s" field;
|PIndex(idx,lval_prime) -> print_pc_lval out lval_prime; Format.fprintf out "["; print_pc_expr out idx; Format.fprintf out "]"

and print_pc_expr out (exp: pc_expr) = match exp with
  PCst(cst) -> Format.fprintf out "PCst(";print_pc_cst out cst;Format.fprintf out ")";
  |PBinop(binop,e1,e2) -> Format.fprintf out "PBinop(";print_pc_binop out binop;
                          Format.fprintf out ",";print_pc_expr out e1;
                          Format.fprintf out ",";print_pc_expr out e2;Format.fprintf out ")";
  |PUnop(unop,exp) -> Format.fprintf out "PUnop(";print_pc_unop out unop;
                      Format.fprintf out ",";print_pc_expr out exp;Format.fprintf out ")";
  |PUndef -> Format.fprintf out "PUndef";
  |PLval(lval) -> Format.fprintf out "PLoad(PLVal(";print_pc_lval out lval;
                  Format.fprintf out ")";Format.fprintf out ")";
  |PArg((vname)) -> Format.fprintf out "PArg(%s)" vname;
  |PAddr((ptr,_)) -> Format.fprintf out "PAddr(%s)" ptr

let rec print_pc_instr_type out (i_type: pc_instr) = match i_type with
  PStore(e,lval) -> Format.fprintf out "PStore(";print_pc_expr out e;Format.fprintf out ",PLVal(";print_pc_lval out lval;
                    Format.fprintf out ")";Format.fprintf out ")";
  |PCall(fname,args) -> Format.fprintf out "PCall(%s," fname; (List.iter (print_pc_expr out) args);Format.fprintf out ")";
  |PIf(e, l1, l2) -> Format.fprintf out "PIf(";print_pc_expr out e;Format.fprintf out ",";
                     (List.iter (print_pc_instr out) l1);Format.fprintf out ",";
                     (List.iter (print_pc_instr out) l2);Format.fprintf out ")";
  |PWhile(l,break_lbl) -> Format.fprintf out "PWhile(";(List.iter (print_pc_instr out) l);Format.fprintf out ";";
                          Format.fprintf out "%s)" break_lbl;
  |PReturn(e) -> Format.fprintf out "PReturn(";print_pc_expr out e;Format.fprintf out ")";
  |PDecl(e,(ptr,_)) -> Format.fprintf out "PDecl(";print_pc_expr out e;Format.fprintf out ",%s)" ptr;
  |PCopy(e,(ptr_dst,_)) -> Format.fprintf out "PCopy(";print_pc_expr out e;Format.fprintf out ",%s)" ptr_dst;
  |PPop -> Format.fprintf out "PPop()";
  |PRetAttr(lval) -> Format.fprintf out "PRetAttr(PLVal(";print_pc_lval out lval;
                     Format.fprintf out ")";Format.fprintf out ")";
  |PGoto(lbl) -> Format.fprintf out "PGoto(%s)" lbl;
  |PLabel(lbl) -> Format.fprintf out "PStmt(%s)" lbl;
  |PInitDone -> Format.fprintf out "PInitDone";
  |PAwaitInit -> Format.fprintf out "PAwaitInit"
  |PSkip -> Format.fprintf out "PSkip"
  |PInitArray(size,(ptr,_)) -> Format.fprintf out "PInitArray(%d,%s)" size ptr


and print_pc_instr out (i: pc_instr) =
  Format.fprintf out "INSTR : type "; print_pc_instr_type out i; Format.fprintf out "\n"

let print_pc_procedure out (proc: pc_procedure) =
  Format.fprintf out "PROCEDURE : %s\n" proc.pc_procedure_name;
  Format.fprintf out "PROCEDURE ARGS : "; (List.iter (print_pc_var out) proc.pc_procedure_args);Format.fprintf out "\n";
  Format.fprintf out "PROCEDURE VARS : "; (List.iter (print_pc_var out) proc.pc_procedure_vars);Format.fprintf out "\n";
  Format.fprintf out "PROCEDURE BODY : "; (List.iter (print_pc_instr out) proc.pc_procedure_body);Format.fprintf out "\n"

let print_pc_process out (proc: pc_process) =
  Format.fprintf out "PROC : %s\n" proc.pc_process_name;
  Format.fprintf out "PROC VAR : "; (List.iter (print_pc_var out) proc.pc_process_vars);Format.fprintf out "\n";
  Format.fprintf out "PROC BODY : "; (List.iter (print_pc_instr out) proc.pc_process_body);Format.fprintf out "\n"

let print_prog out (prog: pc_prog) =
  Format.fprintf out "PROG : %s\n" prog.pc_prog_name;
  Format.fprintf out "GLOB VAR : "; (List.iter (print_pc_var out) (List.map (fun ((a,_),_) -> a) prog.pc_glob_var));Format.fprintf out "\n";
  Format.fprintf out "PROCESSUS : "; (List.iter (print_pc_process out) prog.pc_processus);Format.fprintf out "\n";
  Format.fprintf out "PROCEDURES : "; (List.iter (print_pc_procedure out) prog.pc_procedures);Format.fprintf out "\n"