(**
    Dump a PlusCal program.
**)

open Pc
open Pc_utils
open Invariants_utils

(**
  Type representing information to be dumped.
  @param label string representing the label.
  @param proc_name string representing the procedure name.
  @param line integer representing the line number.
  @param indent_space string representing the indentation space.
**)
type dump_info = string * string * int * string


(**
  Outputs the variable declaration for a given procedure variable.
  @param out formatter to output the variable declaration
  @param proc_name name of the procedure
  @param v_name procedure variable to be declared
**)
let dump_proc_var out (proc_name: string) ((v_name): pc_var) =
  Format.fprintf out "    %s = [loc |-> \"stack\", fp |-> Len(my_stack), offs |-> 0];\n" (vname_to_string proc_name v_name)


(**
  Outputs the string representation of a PlusCal argument of a procedure.
  @param proc_name name of the procedure to which the variable belongs.
  @param out formatter to which the variable's string representation is printed.
  @param v PlusCal arg to be printed.
**)
let dump_arg (proc_name: string) out (v: pc_var) =  Format.fprintf out "%s" (arg_to_string proc_name v)


(**
  Outputs the string representation of the given
  PlusCal binary operator [b] to the formatter [out].
  @param out formatter to which the binary operator will be printed.
  @param b PlusCal binary operator to be printed. It can be one of the following:

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
  |_ -> Format.fprintf out "Error: dump_pc_binop: non-ptr operation expected"


(**
  Outputs the PlusCal representation of a unary operator [u] to the formatter [out].
  @param out formatter to print to.
  @param u unary operator to be printed. It can be one of the following:
**)
let dump_pc_unop out (u: pc_unop) = match u with
  | PMinus -> Format.fprintf out "-";
  | PNot -> Format.fprintf out "~"
  | PBnot -> Format.fprintf out "Not"


(**
  Outputs the representation of a pointer binary operation to the given formatter.
  @param out formatter to output the representation.
  @param proc_name name of the procedure containing the expressions.
  @param b binary operation to be represented.
  @param e1 first expression (expected to be a pointer lvalue).
  @param e2 second expression.
**)
let rec dump_pc_binop_ptr out (proc_name: string) (b: pc_binop) (e1: pc_expr) (e2: pc_expr) =
  (match e1 with
    | PLval lval ->
      let ptr_string = string_of_pc_lval proc_name lval in
        (match b with
          (* Our stack grows backward, we should invert arithmetic operations *)
          | PAddPI -> Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs-"
                      ptr_string ptr_string ptr_string;
                      dump_pc_expr proc_name out e2;
                      Format.fprintf out "]";

          | PSubPI -> Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs+"
                      ptr_string ptr_string ptr_string;
                      dump_pc_expr proc_name out e2;
                      Format.fprintf out "]";

          | PSubPP -> (match e2 with
                        | PLval lval2 ->
                          let ptr2_string = string_of_pc_lval proc_name lval2 in
                          Format.fprintf out "[loc |-> %s.loc, fp |-> %s.fp, offs |-> %s.offs+%s.offs]"
                          ptr_string ptr_string ptr_string ptr2_string;
                        | _ -> Format.fprintf out "Error: dump_pc_binop_ptr: second lval operand expected")

          | _ -> Format.fprintf out "Error: dump_pc_binop_ptr: binop should be ptr operator")
    | _ -> Format.fprintf out "Error: dump_pc_binop_ptr: first lval operand expected")


(**
  Outputs the PlusCal representation of a constant [c] to the formatter [out].
  @param proc_name name of the procedure.
  @param out output formatter.
  @param c PlusCal constant to be dumped.
**)
and dump_pc_cst (proc_name: string) out (c: pc_cst) = match c with
  | PInt i -> Format.fprintf out "%s" (string_of_int i);
  | PString s -> Format.fprintf out "\"%s\"" s;

  | PRecord l ->
      Format.fprintf out "[";
      dump_list out l
          (fun out (field_name, exp) ->
          Format.fprintf out "%s |-> " field_name;
          dump_pc_expr proc_name out exp;);
      Format.fprintf out "]"

  | PArray l ->
      Format.fprintf out "<<";
      dump_list out l (fun out exp -> dump_pc_expr proc_name out exp;);
      Format.fprintf out ">>"

  | PEnumItem item_name -> Format.fprintf out "%s" item_name


(**
  Outputs the PlusCal representation of an expression [e] to the formatter [out].
  @param proc_name name of the procedure in which the expression is located.
  @param out formatter where the output string representation of the expression is written.
  @param exp PlusCal expression to be formatted.
**)
and dump_pc_expr (proc_name: string) out (exp: pc_expr) = match exp with
  | PCst cst -> dump_pc_cst proc_name out cst

  | PBinop (binop, e1, e2) ->
      if is_binop_ptr binop then (
        Format.fprintf out "(";
        dump_pc_binop_ptr out proc_name binop e1 e2;
        Format.fprintf out ")"
      ) else (
        Format.fprintf out "(";
        dump_pc_expr proc_name out e1;
        dump_pc_binop out binop;
        dump_pc_expr proc_name out e2;
        Format.fprintf out ")"
      )

  | PUnop (unop, exp) ->
      Format.fprintf out "(";
      dump_pc_unop out unop;
      Format.fprintf out "(";
      dump_pc_expr proc_name out exp;
      Format.fprintf out "))"

  (*Lvalue as an expression
    We need to load the value from PlusCal pointer
    representation of variables, before calling dump_pc_lval*)
  | PLval lval ->
      Format.fprintf out "load(my_stack, ";
      dump_pc_lval out proc_name lval;
      Format.fprintf out ")"

  | PArg v -> dump_arg proc_name out v

  | PUndef -> Format.fprintf out "UNDEF"

  | PAddr lval -> dump_pc_lval out proc_name lval;

(**
  Outpust the PlusCal representation of an lvalue [lval] to the formatter [out].
  @param out formatter to which the output is printed.
  @param proc_name name of the procedure containing the lvalue.
  @param lval PlusCal lvalue to be printed.
**)
and dump_pc_lval out (proc_name: string) (lval: pc_lval) = match lval with
  |PLVar ptr -> Format.fprintf out "%s" (ptr_to_string proc_name ptr);

  |PLoad lval' ->
    Format.fprintf out "load(my_stack,";
    dump_pc_lval out proc_name lval';
    Format.fprintf out ")";

  |PField (field, lval') ->
    Format.fprintf out "[ptr |-> ";
    dump_pc_lval out proc_name lval';
    Format.fprintf out ", ref |-> \"%s\"]" field;

  | PIndex (idx, lval') ->
    (*Add 1 to the index, because TLA sequences indexes begins at 1*)
    let idx_plus_one = add_pc_cst idx 1 in
    Format.fprintf out "[ptr |-> ";
    dump_pc_lval out proc_name lval';
    Format.fprintf out ", ref |-> ";
    dump_pc_expr proc_name out idx_plus_one;
    Format.fprintf out "]";


(**
  Converts a PlusCal expression [exp] into its string representation.
  @param proc_name name of the procedure containing the expression.
  @param exp PlusCal expression to be converted.
  @return [String] representation of the PlusCal expression.

  Useful in string_of_pc_lval
**)
and string_of_pc_expr (proc_name: string) (exp: pc_expr) : string =
    let buffer = Buffer.create 256 in
    let formatter = Format.formatter_of_buffer buffer in
    dump_pc_expr proc_name formatter exp;
    Format.pp_print_flush formatter ();
    Buffer.contents buffer

(**
  Converts a PlusCal lvalue to its string representation.
  @param proc_name name of the procedure.
  @param lval PlusCal lvalue to be converted.
  @return [String] representation of the PlusCal l-value.

  Useful in dump_pc_binop_ptr
**)
and string_of_pc_lval (proc_name: string) (lval: pc_lval) = match lval with
  | PLVar ptr -> "load(my_stack," ^ (ptr_to_string proc_name ptr) ^ ")"
  | PLoad lval' -> "load(my_stack," ^ string_of_pc_lval proc_name lval' ^ ")"
  | PField (field,lval') -> "load(my_stack," ^ string_of_pc_lval proc_name lval' ^ ")"
                                  ^ "." ^ field
  | PIndex (idx,lval') ->
    (*Add 1 to the index, because TLA sequences indexes begins at 1*)
    let idx_plus_one = add_pc_cst idx 1 in
      "load(my_stack," ^ string_of_pc_lval proc_name lval' ^ ")["
            ^ (string_of_pc_expr proc_name idx_plus_one) ^ "]"

(**
  Recursively outputs PlusCal instructions to the given output formatter [out].
  @param out formatter to output the dumped instructions.
  @param info tuple containing the label, procedure name, line number, and indentation level.
  @param instr PlusCal instruction to dump.

  The function uses helper functions.
**)
let rec dump_pc_instr out (info: dump_info) (instr: pc_instr) =
  let label, proc_name, line, indent = info in match instr with
  | PStore (e,lval) ->
      Format.fprintf out "store(";
      dump_pc_expr proc_name out e;
      Format.fprintf out ", ";
      dump_pc_lval out proc_name lval;
      Format.fprintf out ");\n"

  | PRetAttr lval ->
      Format.fprintf out "attr_return(ret, ";
      dump_pc_lval out proc_name lval;
      Format.fprintf out ");\n"

  | PIf (e,l1,l2) ->
      Format.fprintf out "if(";
      dump_pc_expr proc_name out e;
      Format.fprintf out ") then\n";
      (*Print block of instruction*)
      (List.iteri (fun i -> dump_pc_label out
        ((String.concat "" [label;(string_of_int i)]),proc_name,line,add_indent 1 indent)) l1
      );

      (*Print else if there is one*)
      if List.length l2 > 0 then
        Format.fprintf out "%selse\n" indent;
        (List.iteri (fun i -> dump_pc_label out
          ((String.concat "" [label;(string_of_int (List.length l1+i))]),proc_name,line,add_indent 1 indent)) l2
        );
      Format.fprintf out "%send if;\n" indent;

  | PWhile (instrs,lbl) ->
      Format.fprintf out "while(TRUE) do\n";
      (List.iteri (fun i -> dump_pc_label out
        ((String.concat "" [label;(string_of_int i)]),proc_name,line,add_indent 1 indent)) instrs
      );
      Format.fprintf out "%send while;\n" indent;
      Format.fprintf out "\n";
      Format.fprintf out "%s%s\n" indent lbl;
      Format.fprintf out "%sskip;\n" indent;

  | PCall (fname,args) -> Format.fprintf out "call %s(" fname;dump_list out args (dump_pc_expr proc_name);Format.fprintf out ");\n";
  | PReturn e -> Format.fprintf out "push(ret, ";dump_pc_expr proc_name out e;Format.fprintf out ");\n";
  | PDecl (e,ptr) -> Format.fprintf out "decl(";dump_pc_expr proc_name out e;Format.fprintf out ",%s);\n" (ptr_to_string proc_name ptr);
  | PCopy (e,ptr_dst) -> Format.fprintf out "%s := " (ptr_to_string proc_name ptr_dst);dump_pc_expr proc_name out e;Format.fprintf out ";\n";
  | PPop -> Format.fprintf out "pop(my_stack);\n";
  | PGoto lbl -> Format.fprintf out "goto %s;\n" (remove_last_char lbl); (* lbl = "label:", remove the ":"*)
  | PLabel _ -> Format.fprintf out "skip;\n"; (*Label are printed in dump_pc_label*)
  | PInitDone -> Format.fprintf out "initDone := TRUE;\n"
  | PAwaitInit -> Format.fprintf out "await initDone = TRUE;\n"
  | PSkip -> Format.fprintf out "skip;"
  | PInitArray (size,ptr) -> Format.fprintf out "call init_array(%d,%s);\n" size (ptr_to_string proc_name ptr)

(**
  Outputs a label for a PlusCal instruction [i] to the output formatter [out].
  @param out formatter to print the label to.
  @param info tuple containing the generated label string, procedure name (unused), the line number, and the indentation string.
  @param i PlusCal instruction to print the label for.

  The label is either a Frama-C label if present, or a generated label
  based on the line number and a generated label string.
**)
and dump_pc_label out (info: dump_info) (i: pc_instr) =
  let gen_label, _, line, indent = info in

  (*Computed the label to print, frama-c label if there is one, or generated label*)
  let printed_label = match i with
    | PLabel label -> label
    | _ -> String.concat "" ["Line";(string_of_int line);"_";gen_label;":"]
  in

  Format.fprintf out "%s%s\n" indent printed_label;
  Format.fprintf out "%s" indent;
  dump_pc_instr out info i;
  Format.fprintf out "\n"


(**
  Outputs the PlusCal procedure [proc] to the formatter [out].

  @param out formatter to output the procedure to.
  @param proc PlusCal procedure to be dumped.

  The function performs the following steps:
  - Outputs the procedure signature, including the procedure name and arguments.
  - Outputs the variables section, including procedure arguments and local variables.
  - Outputs the core of the procedure, including the procedure body and the return statement.
**)
let dump_pc_procedure out (proc: pc_procedure) =
  (*Procedure signature*)
  Format.fprintf out "procedure %s(" proc.pc_procedure_name;
  dump_list out proc.pc_procedure_args (dump_arg proc.pc_procedure_name);
  Format.fprintf out ")\n";

  (*Variables section*)
  Format.fprintf out "variables\n";
  (List.iter (dump_proc_var out proc.pc_procedure_name) proc.pc_procedure_args);
  (List.iter (dump_proc_var out proc.pc_procedure_name) proc.pc_procedure_vars);

  (*Core of the procedure*)
  Format.fprintf out "begin\n";
  (List.iteri (fun i ->
    dump_pc_label out (proc.pc_procedure_name, proc.pc_procedure_name, i, (string_of_indent 1)))
  proc.pc_procedure_body);
  Format.fprintf out "    %s:\n" (String.concat "" ["End_";proc.pc_procedure_name]);
  Format.fprintf out "    return;\n";
  Format.fprintf out "end procedure;\n";
  Format.fprintf out "\n"


(**
  Outputs the PlusCal process [proc] to the formatter [out].
  @param out formatter to output the process definition.
  @param proc PlusCal process to be dumped, which includes the process name, set, variables, and body.

  The function performs the following steps:
  - Outputs the process header with the process name and set.
  - Outputs the variables section.
  - Outputs the core of the process, iterating over the process body and dumping each label.
  - Outputs the process end statement.
**)
let dump_pc_process out (proc: pc_process) =
  Format.fprintf out "fair process %s \\in %s\n" proc.pc_process_name proc.pc_process_set;

  (*Variables section*)
  Format.fprintf out "variables\n";
  Format.fprintf out "\n";
  (List.iter (dump_proc_var out proc.pc_process_name) proc.pc_process_vars);

  (*Core of the process*)
  Format.fprintf out "begin\n";
  (List.iteri (fun i -> dump_pc_label out (proc.pc_process_name,proc.pc_process_name,i,(string_of_indent 1))) proc.pc_process_body);
  Format.fprintf out "end process;\n"


(**
  Outputs the representation of a global variable to the given formatter.
  @param out formatter to write the output to.
  @param gv_name name of the global variable to be dumped.
**)
let dump_glob_var out (gv_name: pc_var) =
  Format.fprintf out "    %s = [loc |-> \"mem\", fp |-> 0, offs |-> 0];\n" (vname_to_string "glob" gv_name)

(**
  Outputs the string constant [cst] to the formatter [out].
  @param out formatter to write the string constant to.
  @param cst string constant to be written.

  Useful as a function to fold on list
**)
let dump_constant out (cst: string) =
  Format.fprintf out "%s" cst


(**
  Outputs a PlusCal program [prog] to the given output formatter [out].

  @param out output formatter where the program will be written.
  @param prog PlusCal program to be dumped.

  The function performs the following steps:
  - Prints the module header with the program name.
  - Prints the EXTENDS section with the necessary TLA+ modules.
  - Prints the CONSTANT section with the constants used in the program.
  - Prints the global variables section, including initialization of memory and temporary variables.
  - Defines helper functions and macros for stack operations and memory management.
  - Defines procedures for initializing arrays and stacks.
  - Dumps the procedures and processes defined in the PlusCal program.
  - Closes the algorithm block.
**)
let dump_prog out (prog: pc_prog) =
  Format.fprintf out "------------------------------ MODULE %s ------------------------------\n" prog.pc_prog_name;
  Format.fprintf out "EXTENDS Integers, FiniteSets, Sequences, Bitwise\n";

  (*Constant section*)
  Format.fprintf out "CONSTANT ";
  dump_list out (List.map (fun proc -> proc.pc_process_set) prog.pc_processus) (dump_constant);
  Format.fprintf out ",UNDEF,\n";
  Format.fprintf out "         ";
  dump_list out (List.map (fun (cst_name,_) -> cst_name) prog.pc_constants) (dump_constant);
  Format.fprintf out "\n";
  Format.fprintf out "\n";

  (*Global variables*)
  Format.fprintf out "(*--algorithm %s\n" prog.pc_prog_name;
  Format.fprintf out "variables\n";
  Format.fprintf out "    mem = <<>>;\n";
  Format.fprintf out "    tmpArrayFill = 0;\n";
  Format.fprintf out "    initDone = FALSE;\n";
  Format.fprintf out "\n";
  List.iter (dump_glob_var out) (List.map (fun ((a,_),_) -> a) prog.pc_glob_var);
  Format.fprintf out "\n";

  (*Define section*)
  Format.fprintf out "define\n";
  Format.fprintf out "    RECURSIVE load(_,_)\n";
  Format.fprintf out "    load(stk, ptr) == IF \"ptr\" \\in DOMAIN ptr THEN\n";
  Format.fprintf out "                         load(stk, ptr.ptr)[ptr.ref]\n";
  Format.fprintf out "                      ELSE\n";
  Format.fprintf out "                         IF ptr.loc = \"stack\"\n";
  Format.fprintf out "                         THEN stk[Len(stk) - (ptr.fp + ptr.offs)]\n";
  Format.fprintf out "                         ELSE mem[Len(mem) - ptr.offs]\n";
  Format.fprintf out "\n";
  Format.fprintf out "    RECURSIVE idx_seq(_,_)\n";
  Format.fprintf out "    idx_seq(stk, ptr) == IF \"ptr\" \\in DOMAIN ptr THEN\n";
  Format.fprintf out "                         idx_seq(stk, ptr.ptr) \\o <<ptr.ref>>\n";
  Format.fprintf out "                      ELSE\n";
  Format.fprintf out "                         IF ptr.loc = \"stack\"\n";
  Format.fprintf out "                         THEN <<\"stack\", Len(stk) - (ptr.fp + ptr.offs)>>\n";
  Format.fprintf out "                         ELSE <<\"mem\", Len(mem) - ptr.offs>>\n";
  Format.fprintf out "\n";
  Format.fprintf out "    RECURSIVE update_stack(_,_,_)\n";
  Format.fprintf out "    update_stack(stk, val, seq) == IF seq = <<>>\n";
  Format.fprintf out "                                    THEN val\n";
  Format.fprintf out "                                    ELSE [stk EXCEPT ![seq[1]] = update_stack(stk[seq[1]], val, Tail(seq))]\n";
  Format.fprintf out "end define;\n";
  Format.fprintf out "\n";

  (*Macros and procedures useful for the PlusCal translation*)
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
  Format.fprintf out "  with seq = idx_seq(my_stack, ptr) do\n";
  Format.fprintf out "  if seq[1] = \"stack\"\n";
  Format.fprintf out "    then my_stack := update_stack(my_stack, val, Tail(seq));\n";
  Format.fprintf out "    else mem := update_stack(mem, val, Tail(seq));\n";
  Format.fprintf out "  end if;\n";
  Format.fprintf out "  end with;\n";
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

  (*Procedures*)
  (List.iter (dump_pc_procedure out) (List.rev prog.pc_procedures));

  (*Process*)
  (List.iter (dump_pc_process out) prog.pc_processus);

  Format.fprintf out "end algorithm; *)\n";

  (*Dumps Invariant written in .expect file, if the options is given to Frama-C*)
  let expect_file = Options.ExpectVal.get() in
  if String.length expect_file > 0 then (
    Format.fprintf out "Inv == /\\ TRUE\n";
    let expect_entries = parse_expect_file expect_file in
    List.iter (
      fun entry -> match entry with
      (* Checks variable value at "Check" label of a procedure *)
      | Ok Global (global_entry, (proc_check_name, proc_check_id)) ->
          Format.fprintf out "       /\\ (pc[%i] = \"Check_%s\" => load(mem, %s_ptr_glob) = %s)\n"
            proc_check_id proc_check_name global_entry.var_name global_entry.expected_val;

      | Ok Local (local_entry, (proc_check_name, proc_check_id)) ->
          Format.fprintf out "       /\\ (pc[%i] = \"Check_%s\" => load(my_stack[%i], %s_ptr_%s[%i]) = %s)\n"
            proc_check_id proc_check_name local_entry.proc_id local_entry.var_name
            local_entry.proc_name local_entry.proc_id local_entry.expected_val;

      | Error e -> Format.fprintf out "%s" e
    ) expect_entries
  ) else ();

  Format.fprintf out "============================================================================="
