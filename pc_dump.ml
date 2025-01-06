open Pc
open Pc_utils

type dump_info = string * string * int * string (* label, proc_name, line, indent_space*)

let dump_proc_var out (proc_name: string) ((v_name,_): pc_var) =
  Format.fprintf out "    %s = [loc |-> \"stack\", fp |-> Len(my_stack), offs |-> 0];\n" (vname_to_string proc_name v_name)

let dump_arg (proc_name: string) out (v: pc_var) =  Format.fprintf out "%s" (arg_to_string proc_name v)

let dump_pc_binop out (b: pc_binop) = match b with
  PAdd -> Format.fprintf out "+";
  | PSub -> Format.fprintf out "-";
  | PMul -> Format.fprintf out "*";
  | PDiv -> Format.fprintf out "/";
  | PMod -> Format.fprintf out "mod";
  | PLt -> Format.fprintf out "<";
  | PGt -> Format.fprintf out ">";
  | PLe -> Format.fprintf out "<=";
  | PGe -> Format.fprintf out ">=";
  | PEq -> Format.fprintf out "=";
  | PNe -> Format.fprintf out "/=";
  | PLand -> Format.fprintf out "/\\";
  | PLor -> Format.fprintf out "\\/";
  |_ -> Format.fprintf out "Error: non-ptr op expected"

let dump_pc_unop out (u: pc_unop) = match u with
  |PMinus -> Format.fprintf out "-";
  |PNot -> Format.fprintf out "~"

let rec dump_pc_cst (proc_name: string) out (c: pc_cst) = match c with
  |PInt(i) -> Format.fprintf out "%s" (string_of_int i);
  |PString(s) -> Format.fprintf out "\"%s\"" s;
  |PRecord l -> Format.fprintf out "[";
                dump_list out l (fun out (field_name, exp) ->
                  Format.fprintf out "%s |-> " field_name;
                  dump_pc_expr proc_name out exp;);
                Format.fprintf out "]"

and dump_pc_expr (proc_name: string) out (exp: pc_expr) = match exp with
  PCst(cst) -> dump_pc_cst proc_name out cst;
  |PPtr(ptr) -> Format.fprintf out "%s" (ptr_to_string proc_name ptr);
  |PBinop(binop,e1,e2) -> (match is_binop_ptr binop with
                            |true ->
                              Format.fprintf out "(";
                              dump_pc_binop_ptr out proc_name binop e1 e2;
                              Format.fprintf out ")";
                            |false ->
                              Format.fprintf out "(";dump_pc_expr proc_name out e1;
                              dump_pc_binop out binop;
                              dump_pc_expr proc_name out e2;Format.fprintf out ")";)
  |PUnop(unop,exp) -> Format.fprintf out "(";dump_pc_unop out unop;
                      dump_pc_expr proc_name out exp;Format.fprintf out ")";
  |PUndef -> Format.fprintf out "UNDEF";
  |PLoad(ptr) -> Format.fprintf out "load(self,%s)" (ptr_to_string proc_name ptr)
  |PArg(v) -> dump_arg proc_name out v

(* Our stack grows backward -> should invert arithmetic operations *)
and dump_pc_binop_ptr out (proc_name: string) (b: pc_binop) (e1: pc_expr) (e2: pc_expr) =
  (match e1 with
    |PPtr(ptr) -> let ptr_string = (ptr_to_string proc_name ptr) in
                  (match b with
                    | PAddPI -> Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs-" ptr_string ptr_string ptr_string;
                                dump_pc_expr proc_name out e2; Format.fprintf out "]";
                    | PSubPI -> Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs+" ptr_string ptr_string ptr_string;
                                dump_pc_expr proc_name out e2; Format.fprintf out "]";
                    | PSubPP -> (match e2 with
                                  |PPtr(ptr2) -> let ptr2_string = (ptr_to_string proc_name ptr2) in
                                    Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs+%s.offs]" ptr_string ptr_string ptr_string ptr2_string;
                                  |_ -> Format.fprintf out "Error: ptr snd op expected")
                    | _ -> Format.fprintf out "Error: ptr fst op expected")
    |_ -> Format.fprintf out "Error: ptr operand expected")

let rec dump_pc_instr_type out (info: dump_info) (i_type: pc_instr) =
  let label,proc_name,line,indent = info in match i_type with
  PStore(e,ptr) -> Format.fprintf out "store(";dump_pc_expr proc_name out e;Format.fprintf out ",%s);\n" (ptr_to_string proc_name ptr);
  |PStorePtr(e,ptr) -> Format.fprintf out "%s := " (ptr_to_string proc_name ptr);dump_pc_expr proc_name out e;Format.fprintf out ";\n";
  |PCall(fname,args) -> Format.fprintf out "call %s(" fname;dump_list out args (dump_pc_expr proc_name);Format.fprintf out ");\n";
  |PIf(e,l1,l2) -> Format.fprintf out "if(";dump_pc_expr proc_name out e;Format.fprintf out ") then\n";
                    (List.iteri (fun i -> dump_pc_instr out
                      ((String.concat "" [label;(string_of_int i)]),proc_name,line,add_indent 1 indent)) l1);
                    if List.length l2 > 0 then
                      Format.fprintf out "%selse\n" indent;
                      (List.iteri (fun i -> dump_pc_instr out
                        ((String.concat "" [label;(string_of_int (List.length l1+i))]),proc_name,line,add_indent 1 indent)) l2);
                    Format.fprintf out "%send if;\n" indent;
  |PWhile -> Format.fprintf out "while;\n";
  |PReturn(e) -> Format.fprintf out "push(ret, ";dump_pc_expr proc_name out e;Format.fprintf out ");\n";
  |PDecl(e,ptr) -> Format.fprintf out "decl(";dump_pc_expr proc_name out e;Format.fprintf out ",%s);\n" (ptr_to_string proc_name ptr);
  |PPop -> Format.fprintf out "pop(my_stack);\n";
  |PRetAttr(ptr) -> Format.fprintf out "attr_return(ret, %s);\n" (ptr_to_string proc_name ptr);
  |PGoto(lbl) -> Format.fprintf out "goto %s;\n" (remove_last_char lbl); (* lbl="label:", remove the ":"*)
  |PLabel(_) -> Format.fprintf out "skip;\n";
  |PInitDone -> Format.fprintf out "initDone := TRUE;\n"
  |PAwaitInit -> Format.fprintf out "await initDone = TRUE;\n"

and dump_pc_instr out (info: dump_info) (i: pc_instr) =
  let label,_,line,indent = info in
  let lbl = match i with |PLabel(l) -> l |_ -> String.concat "" ["Line";(string_of_int line);"_";label;":"] in
  Format.fprintf out "%s%s\n" indent lbl;
  Format.fprintf out "%s" indent;dump_pc_instr_type out info i;
  Format.fprintf out "\n"

let dump_pc_procedure out (proc: pc_procedure) =
  Format.fprintf out "procedure %s(" proc.pc_procedure_name;dump_list out proc.pc_procedure_args (dump_arg proc.pc_procedure_name);Format.fprintf out ")\n";
  Format.fprintf out "variables\n";
  (List.iter (dump_proc_var out proc.pc_procedure_name) (List.filter (fun (_,bool_ptr) -> not bool_ptr) proc.pc_procedure_args));
  (List.iter (dump_proc_var out proc.pc_procedure_name) proc.pc_procedure_vars);
  Format.fprintf out "begin\n";
  (List.iteri (fun i -> dump_pc_instr out (proc.pc_procedure_name,proc.pc_procedure_name,i,(string_of_indent 1))) proc.pc_procedure_body);
  Format.fprintf out "    %s:\n" (String.concat "" ["End_";proc.pc_procedure_name]);
  Format.fprintf out "    return;\n";
  Format.fprintf out "end procedure;\n";
  Format.fprintf out "\n"

let dump_pc_process out (proc: pc_process) =
  Format.fprintf out "fair process %s \\in %s\n" proc.pc_process_name proc.pc_process_set;
  Format.fprintf out "variables\n";
  Format.fprintf out "\n";
  (List.iter (dump_proc_var out proc.pc_process_name) proc.pc_process_vars);
  Format.fprintf out "begin\n";
  (List.iteri (fun i -> dump_pc_instr out (proc.pc_process_name,proc.pc_process_name,i,(string_of_indent 1))) proc.pc_process_body);
  Format.fprintf out "end process;\n"

let dump_glob_var out ((gv_name,_): pc_var) =
  Format.fprintf out "    %s = [loc |-> \"mem\", fp |-> 0, offs |-> 0];\n" (vname_to_string "glob" gv_name)

let dump_constant out (cst: string) =
  Format.fprintf out "%s" cst

let dump_prog out (prog: pc_prog) =
  Format.fprintf out "------------------------------ MODULE %s ------------------------------\n" prog.pc_prog_name;
  Format.fprintf out "EXTENDS Integers, FiniteSets, Sequences\n";
  Format.fprintf out "CONSTANT ";
  dump_list out (List.map (fun proc -> proc.pc_process_set) prog.pc_processus) (dump_constant);
  Format.fprintf out ",UNDEF\n";
  Format.fprintf out "\n";
  Format.fprintf out "(*--algorithm %s\n" prog.pc_prog_name;
  Format.fprintf out "variables\n";
  Format.fprintf out "    mem = <<>>;\n";
  Format.fprintf out "    tmpArrayFill = 0;\n";
  Format.fprintf out "    initDone = FALSE;\n";
  Format.fprintf out "\n";
  List.iter (dump_glob_var out) (List.map (fun (a,_) -> a) prog.pc_glob_var);
  Format.fprintf out "\n";
  Format.fprintf out "define\n";
  Format.fprintf out "    load(self, ptr) == IF ptr.loc = \"stack\"\n";
  Format.fprintf out "                        THEN my_stack[self][Len(my_stack[self]) - (ptr.fp + ptr.offs)]\n";
  Format.fprintf out "                        ELSE mem[Len(mem) - ptr.offs]\n";
  Format.fprintf out "end define;\n";
  Format.fprintf out "\n";
  Format.fprintf out "macro push(my_stack, val) begin\n";
  Format.fprintf out "    my_stack := <<val>> \\o my_stack;\n";
  Format.fprintf out "end macro;\n";
  Format.fprintf out "\n";
  Format.fprintf out "macro pop(my_stack) begin\n";
  Format.fprintf out "    my_stack := Tail(my_stack);\n";
  Format.fprintf out "end macro;\n";
  Format.fprintf out "\n";
  Format.fprintf out "macro decl(val, ptr) begin\n";
  Format.fprintf out "    if ptr.loc = \"stack\" then\n";
  Format.fprintf out "        push(my_stack, val);\n";
  Format.fprintf out "        ptr.offs := Len(my_stack) - ptr.fp - 1;\n";
  Format.fprintf out "    else\n";
  Format.fprintf out "        push(mem, val);\n";
  Format.fprintf out "        ptr.offs := Len(mem) - 1;\n";
  Format.fprintf out "    end if;\n";
  Format.fprintf out "end macro;\n";
  Format.fprintf out "\n";
  Format.fprintf out "macro store(val, ptr) begin\n";
  Format.fprintf out "    if ptr.loc = \"stack\" then\n";
  Format.fprintf out "        my_stack[Len(my_stack) - (ptr.fp + ptr.offs)] := val;\n";
  Format.fprintf out "    else\n";
  Format.fprintf out "        mem[Len(mem) - ptr.offs] := val;\n";
  Format.fprintf out "    end if;\n";
  Format.fprintf out "end macro;\n";
  Format.fprintf out "\n";
  Format.fprintf out "macro attr_return(ret, ptr) begin\n";
  Format.fprintf out "    store(Head(ret), ptr);\n";
  Format.fprintf out "    pop(ret);\n";
  Format.fprintf out "end macro;\n";
  Format.fprintf out "\n";
  Format.fprintf out "procedure stacks_init()\n";
  Format.fprintf out "variables\n";
  Format.fprintf out "    my_stack = <<>>;\n";
  Format.fprintf out "    ret = <<>>;\n";
  Format.fprintf out "begin\n";
  Format.fprintf out "    InitStack:\n";
  Format.fprintf out "    return;\n";
  Format.fprintf out "end procedure;\n";
  Format.fprintf out "\n";
  (List.iter (dump_pc_procedure out) (List.rev prog.pc_procedures));
  (List.iter (dump_pc_process out) prog.pc_processus);
  Format.fprintf out "end algorithm; *)\n";
  Format.fprintf out "============================================================================="
