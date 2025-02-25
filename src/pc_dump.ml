(**
    Dump a PlusCal program.
**)

open Pc
open Pc_utils

(**
  Type representing information to be dumped.
  - @param label string representing the label.
  - @param proc_name string representing the procedure name.
  - @param line integer representing the line number.
  - @param indent_space string representing the indentation space.
**)
type dump_info = string * string * int * string


(**
  Outputs the variable declaration for a given procedure variable.
  - @param out formatter to output the variable declaration
  - @param proc_name name of the procedure
  - @param v_name procedure variable to be declared
**)
let dump_proc_var out (proc_name: string) ((v_name): pc_var) =
  Format.fprintf out "    %s = [loc |-> \"stack\", fp |-> Len(my_stack), offs |-> 0];\n" (vname_to_string proc_name v_name)


(**
  Outputs the string representation of a PlusCal argument of a procedure.
  - @param proc_name name of the procedure to which the variable belongs.
  - @param out formatter to which the variable's string representation is printed.
  - @param v PlusCal arg to be printed.
**)
let dump_arg (proc_name: string) out (v: pc_var) =  Format.fprintf out "%s" (arg_to_string proc_name v)


(**
  Prints the string representation of the given
  PlusCal binary operator [b] to the formatter [out].
  - @param out formatter to which the binary operator will be printed.
  - @param b PlusCal binary operator to be printed. It can be one of the following:

  This function does not handle the pointer operations.
**)
let dump_pc_binop out (b: pc_binop) = match b with
  | PAdd -> Format.fprintf out "+";
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
  | PShiftL -> Format.fprintf out "* 2^";
  | PShiftR -> Format.fprintf out "\\div 2^";
  | PBand -> Format.fprintf out "&";
  | PBor -> Format.fprintf out "|";
  | PBxor -> Format.fprintf out "^^";
  |_ -> Format.fprintf out "Error dump_pc_binop: non-ptr operation expected"

let dump_pc_unop out (u: pc_unop) = match u with
  |PMinus -> Format.fprintf out "-";
  |PNot -> Format.fprintf out "~"
  |PBnot -> Format.fprintf out "Not"

let rec dump_pc_cst (proc_name: string) out (c: pc_cst) = match c with
  |PInt(i) -> Format.fprintf out "%s" (string_of_int i);
  |PString(s) -> Format.fprintf out "\"%s\"" s;
  |PRecord l -> Format.fprintf out "[";
                dump_list out l (fun out (field_name, exp) ->
                  Format.fprintf out "%s |-> " field_name;
                  dump_pc_expr proc_name out exp;);
                Format.fprintf out "]"
  |PArray l -> Format.fprintf out "<<";
    dump_list out l (fun out exp -> dump_pc_expr proc_name out exp;);
    Format.fprintf out ">>"
  |PEnumItem item_name -> Format.fprintf out "%s" item_name

and dump_pc_expr (proc_name: string) out (exp: pc_expr) = match exp with
  | PCst(cst) -> dump_pc_cst proc_name out cst
  | PBinop(binop, e1, e2) -> (match is_binop_ptr binop with
                              | true ->
                                  Format.fprintf out "(";
                                  dump_pc_binop_ptr out proc_name binop e1 e2;
                                  Format.fprintf out ")"
                              | false ->
                                  Format.fprintf out "(";
                                  dump_pc_expr proc_name out e1;
                                  dump_pc_binop out binop;
                                  dump_pc_expr proc_name out e2;
                                  Format.fprintf out ")")
  | PUnop(unop, exp) ->
      Format.fprintf out "(";
      dump_pc_unop out unop;
      Format.fprintf out "(";
      dump_pc_expr proc_name out exp;
      Format.fprintf out "))"
  | PUndef -> Format.fprintf out "UNDEF"
  | PLval(lval) -> (match lval with
                    | PLVar(ptr) -> Format.fprintf out "load(my_stack, %s)" (ptr_to_string proc_name ptr)
                    | PLoad(lval_prime) ->
                        Format.fprintf out "load(my_stack, load(my_stack, ";
                        dump_pc_lval out proc_name lval_prime;
                        Format.fprintf out "))"
                    | PField(field, lval_prime) ->
                        Format.fprintf out "load(my_stack, ";
                        dump_pc_lval out proc_name lval_prime;
                        Format.fprintf out ").%s" field
                    | PIndex(idx, lval_prime) ->
                        Format.fprintf out "load(my_stack, ";
                        dump_pc_lval out proc_name lval_prime;
                        Format.fprintf out ")[";
                        dump_pc_expr proc_name out idx;
                        Format.fprintf out "]")
  | PArg(v) -> dump_arg proc_name out v
  | PAddr(lval) -> (match lval with
                    | PLVar(ptr) -> Format.fprintf out "%s" (ptr_to_string proc_name ptr)
                    | PLoad(lval_prime) ->
                        Format.fprintf out "load(my_stack, ";
                        dump_pc_lval out proc_name lval_prime;
                        Format.fprintf out ")"
                    | _ -> Format.fprintf out "Error: lval not treated")

and dump_pc_addr_expr proc_name out lval = match lval with
  | PLVar(ptr) -> Format.fprintf out "%s" (ptr_to_string proc_name ptr)
  | PLoad(lval_prime) ->
    Format.fprintf out "load(my_stack,";
    dump_pc_lval out proc_name lval_prime;
    Format.fprintf out ")"
  | _ -> Format.fprintf out "Error: lval not treated"

and dump_pc_lval out (proc_name: string) (lval: pc_lval) = match lval with
  |PLVar(ptr) -> Format.fprintf out "%s" (ptr_to_string proc_name ptr);
  |PLoad(lval_prime) -> Format.fprintf out "load(my_stack,";dump_pc_lval out proc_name lval_prime;Format.fprintf out ")";
  |PField(field,lval_prime) -> Format.fprintf out "load(my_stack,";dump_pc_lval out proc_name lval_prime;
                               Format.fprintf out ")";Format.fprintf out ".%s" field;
  |PIndex(idx,lval_prime) -> Format.fprintf out "load(my_stack,";
                             dump_pc_lval out proc_name lval_prime;
                             Format.fprintf out ")";
                             Format.fprintf out "[";
                            (dump_pc_expr proc_name out idx);
                            Format.fprintf out "]"

and string_of_pc_expr (proc_name: string) (exp: pc_expr) : string =
    let buffer = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer buffer in
    dump_pc_expr proc_name formatter exp;
    Format.pp_print_flush formatter ();
    Buffer.contents buffer

and string_of_pc_lval (proc_name: string) (lval: pc_lval) = match lval with
  |PLVar(ptr) -> "load(my_stack,"^(ptr_to_string proc_name ptr)^")"
  |PLoad(lval_prime) -> "load(my_stack,"^string_of_pc_lval proc_name lval_prime^")"
  |PField(field,lval_prime) -> "load(my_stack,"^string_of_pc_lval proc_name lval_prime^")"^"."^field
  |PIndex(idx,lval_prime) -> "load(my_stack,"^string_of_pc_lval proc_name lval_prime^")["^(string_of_pc_expr proc_name idx)^"]"

(* Our stack grows backward -> should invert arithmetic operations *)
and dump_pc_binop_ptr out (proc_name: string) (b: pc_binop) (e1: pc_expr) (e2: pc_expr) =
  (match e1 with
    |PLval(lval) -> let ptr_string = string_of_pc_lval proc_name lval in
                  (match b with
                    | PAddPI -> Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs-" ptr_string ptr_string ptr_string;
                                dump_pc_expr proc_name out e2; Format.fprintf out "]";
                    | PSubPI -> Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs+" ptr_string ptr_string ptr_string;
                                dump_pc_expr proc_name out e2; Format.fprintf out "]";
                    | PSubPP -> (match e2 with
                                  |PLval(lval2) -> let ptr2_string = string_of_pc_lval proc_name lval2 in
                                    Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs+%s.offs]"
                                    ptr_string ptr_string ptr_string ptr2_string;
                                  |_ -> Format.fprintf out "Error: lval snd op expected")
                    | _ -> Format.fprintf out "Error: lval fst op expected")
    |_ -> Format.fprintf out "Error: lval operand expected")

let rec dump_pc_instr_type out (info: dump_info) (i_type: pc_instr) =
  let label,proc_name,line,indent = info in match i_type with
  PStore(e,lval) -> (match lval with
                      |PLVar(ptr) -> Format.fprintf out "store(";dump_pc_expr proc_name out e;Format.fprintf out ",%s);\n" (ptr_to_string proc_name ptr);
                      (* |PLPtr(ptr) -> Format.fprintf out "%s := " (ptr_to_string proc_name ptr);dump_pc_expr proc_name out e;Format.fprintf out ";\n"; *)
                      |PLoad(lval_prime) -> Format.fprintf out "store(";dump_pc_expr proc_name out e;Format.fprintf out ",load(my_stack,";
                                            dump_pc_lval out proc_name lval_prime;
                                            Format.fprintf out "));\n";
                      |PField(field,lval_prime) -> Format.fprintf out "store([load(my_stack,";
                                                   dump_pc_lval out proc_name lval_prime;
                                                   Format.fprintf out ") EXCEPT !.%s = " field;
                                                   dump_pc_expr proc_name out e;
                                                   Format.fprintf out "],";
                                                   dump_pc_lval out proc_name lval_prime;
                                                   Format.fprintf out ");\n";
                      |PIndex(idx,lval_prime) -> Format.fprintf out "store([load(my_stack,";
                                                 dump_pc_lval out proc_name lval_prime;
                                                 Format.fprintf out ") EXCEPT ![";
                                                 dump_pc_expr proc_name out idx;
                                                 Format.fprintf out "] = ";
                                                 dump_pc_expr proc_name out e;
                                                 Format.fprintf out "],";
                                                 dump_pc_lval out proc_name lval_prime;
                                                 Format.fprintf out ");\n")
  |PCall(fname,args) -> Format.fprintf out "call %s(" fname;dump_list out args (dump_pc_expr proc_name);Format.fprintf out ");\n";
  |PIf(e,l1,l2) -> Format.fprintf out "if(";dump_pc_expr proc_name out e;Format.fprintf out ") then\n";
                    (List.iteri (fun i -> dump_pc_instr out
                      ((String.concat "" [label;(string_of_int i)]),proc_name,line,add_indent 1 indent)) l1);
                    if List.length l2 > 0 then
                      Format.fprintf out "%selse\n" indent;
                      (List.iteri (fun i -> dump_pc_instr out
                        ((String.concat "" [label;(string_of_int (List.length l1+i))]),proc_name,line,add_indent 1 indent)) l2);
                    Format.fprintf out "%send if;\n" indent;
  |PWhile(l,lbl) -> Format.fprintf out "while(TRUE) do\n";
                            (List.iteri (fun i -> dump_pc_instr out
                              ((String.concat "" [label;(string_of_int i)]),proc_name,line,add_indent 1 indent)) l);
                    Format.fprintf out "%send while;\n" indent;
                    Format.fprintf out "\n";
                    Format.fprintf out "%s%s\n" indent lbl;
                    Format.fprintf out "%sskip;\n" indent;
  |PReturn(e) -> Format.fprintf out "push(ret, ";dump_pc_expr proc_name out e;Format.fprintf out ");\n";
  |PDecl(e,ptr) -> Format.fprintf out "decl(";dump_pc_expr proc_name out e;Format.fprintf out ",%s);\n" (ptr_to_string proc_name ptr);
  |PCopy(e,ptr_dst) -> Format.fprintf out "%s := " (ptr_to_string proc_name ptr_dst);dump_pc_expr proc_name out e;Format.fprintf out ";\n";
  |PPop -> Format.fprintf out "pop(my_stack);\n";
  |PRetAttr(lval) -> (match lval with
                      |PLVar(ptr) -> Format.fprintf out "attr_return(ret, %s);\n" (ptr_to_string proc_name ptr);
                      (* |PLPtr(ptr) -> Format.fprintf out "attr_return_ptr(ret, %s);\n" (ptr_to_string proc_name ptr); *)
                      |PLoad(lval_prime) -> Format.fprintf out "attr_return(ret, load(my_stack,";
                                            dump_pc_lval out proc_name lval_prime;
                                            Format.fprintf out "));\n";
                      |PField(field,lval_prime) -> Format.fprintf out "store([load(my_stack,";
                                                   dump_pc_lval out proc_name lval_prime;
                                                   Format.fprintf out ") EXCEPT !.%s = Head(ret)]," field;
                                                   dump_pc_lval out proc_name lval_prime;
                                                   Format.fprintf out ");\n";
                                                   Format.fprintf out "pop(ret);\n";
                      |PIndex(idx,lval_prime) -> Format.fprintf out "store([load(my_stack,";
                                                 dump_pc_lval out proc_name lval_prime;
                                                 Format.fprintf out ") EXCEPT ![";
                                                 dump_pc_expr proc_name out idx;
                                                 Format.fprintf out "] = Head(ret)],";
                                                 dump_pc_lval out proc_name lval_prime;
                                                 Format.fprintf out ");\n")
  |PGoto(lbl) -> Format.fprintf out "goto %s;\n" (remove_last_char lbl); (* lbl="label:", remove the ":"*)
  |PLabel(_) -> Format.fprintf out "skip;\n";
  |PInitDone -> Format.fprintf out "initDone := TRUE;\n"
  |PAwaitInit -> Format.fprintf out "await initDone = TRUE;\n"
  |PSkip -> Format.fprintf out "skip;"
  |PInitArray(size,ptr) -> Format.fprintf out "call init_array(%d,%s);\n" size (ptr_to_string proc_name ptr)

and dump_pc_instr out (info: dump_info) (i: pc_instr) =
  let label,_,line,indent = info in
  let lbl = match i with |PLabel(l) -> l |_ -> String.concat "" ["Line";(string_of_int line);"_";label;":"] in
  Format.fprintf out "%s%s\n" indent lbl;
  Format.fprintf out "%s" indent;dump_pc_instr_type out info i;
  Format.fprintf out "\n"

let dump_pc_procedure out (proc: pc_procedure) =
  Format.fprintf out "procedure %s(" proc.pc_procedure_name;dump_list out proc.pc_procedure_args (dump_arg proc.pc_procedure_name);Format.fprintf out ")\n";
  Format.fprintf out "variables\n";
  (List.iter (dump_proc_var out proc.pc_procedure_name) proc.pc_procedure_args);
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

let dump_glob_var out ((gv_name): pc_var) =
  Format.fprintf out "    %s = [loc |-> \"mem\", fp |-> 0, offs |-> 0];\n" (vname_to_string "glob" gv_name)

let dump_constant out (cst: string) =
  Format.fprintf out "%s" cst

let dump_prog out (prog: pc_prog) =
  Format.fprintf out "------------------------------ MODULE %s ------------------------------\n" prog.pc_prog_name;
  Format.fprintf out "EXTENDS Integers, FiniteSets, Sequences, Bitwise\n";
  Format.fprintf out "CONSTANT ";
  dump_list out (List.map (fun proc -> proc.pc_process_set) prog.pc_processus) (dump_constant);
  Format.fprintf out ",UNDEF,\n";
  Format.fprintf out "         ";
  dump_list out (List.map (fun (cst_name,_) -> cst_name) prog.pc_constants) (dump_constant);
  Format.fprintf out "\n";
  Format.fprintf out "\n";
  Format.fprintf out "(*--algorithm %s\n" prog.pc_prog_name;
  Format.fprintf out "variables\n";
  Format.fprintf out "    mem = <<>>;\n";
  Format.fprintf out "    tmpArrayFill = 0;\n";
  Format.fprintf out "    initDone = FALSE;\n";
  Format.fprintf out "\n";
  List.iter (dump_glob_var out) (List.map (fun ((a,_),_) -> a) prog.pc_glob_var);
  Format.fprintf out "\n";
  Format.fprintf out "define\n";
  Format.fprintf out "    load(stk, ptr) == IF ptr.loc = \"stack\"\n";
  Format.fprintf out "                        THEN stk[Len(stk) - (ptr.fp + ptr.offs)]\n";
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
  Format.fprintf out "procedure init_array(size, arr_ptr) begin\n";
  Format.fprintf out "  InitArray:\n";
  Format.fprintf out "  tmpArrayFill := 0;\n";
  Format.fprintf out "  store(<<>>, arr_ptr);\n";
  Format.fprintf out "\n";
  Format.fprintf out "  WhileInitArray:\n";
  Format.fprintf out "  while(tmpArrayFill < size) do\n";
  Format.fprintf out "    store(Append(load(my_stack, arr_ptr), UNDEF), arr_ptr);\n";
  Format.fprintf out "    tmpArrayFill := tmpArrayFill + 1;\n";
  Format.fprintf out "  end while;\n";
  Format.fprintf out "\n";
  Format.fprintf out "  return;\n";
  Format.fprintf out "end procedure;\n";
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
