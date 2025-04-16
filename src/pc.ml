(**
    Type definitions for PlusCal (PC) IR
*)

(** Label of a line *)
type pc_label = string

(** Pointer: name and a flag indicating if it is global *)
type pc_ptr = (string * bool)

(** Variable: name *)
type pc_var = string

(** Binary Operators *)
type pc_binop = 
  | PAdd | PSub | PMul | PDiv | PMod
  | PAddPI | PSubPI | PSubPP
  | PLt | PGt | PLe | PGe | PEq | PNe
  | PLand | PLor
  | PShiftL | PShiftR | PBand | PBor | PBxor

(** Unary Operators *)
type pc_unop = PMinus | PNot | PBnot

(** Constants *)
type pc_cst = 
  | PInt of int
  | PString of string
  | PRecord of (string * pc_expr) list 
  (** Record type: ["a" |-> e1, "b" |-> e2, etc] *)
  
  | PArray of pc_expr list (** Array type: [a, b, etc] *)
  | PEnumItem of string (** Enum item: name of item *)

(* LValue *)
and pc_lval = 
  | PLVar of pc_ptr
  | PLoad of pc_lval 
  (** Load a pointer: pointer to load *)
  
  | PField of (string * pc_lval) 
  (** Field of a record: name of field * pointer of record *)
  
  | PIndex of (pc_expr * pc_lval) 
  (** Index of an array: index expression * pointer of array *)

(* Expression *)
and pc_expr = 
  | PCst of pc_cst (** Constant value: constant *)
  | PArg of pc_var (** Argument of function: argument name *)
  
  | PBinop of pc_binop * pc_expr * pc_expr 
  (** Binary operator: operator * first operand * second operand *)
  
  | PUnop of pc_unop * pc_expr (*** Unary operator: operator * operand *)
  | PUndef (** Undefined for empty declaration *)
  | PLval of pc_lval (* Lvalue *)
  | PAddr of pc_lval (** Address of a variable: lvalue to take address of *)

(** PlusCal Type:
    Useful for TypeOK generation, not implemented yet *)
type pc_type = 
  | PStruct of string * (string list) (** Struct: struct name * (field name list) *)
  | PEnum of string * (string list) (** Enum: enum name * (enum item list) *)

(** Instruction *)
type pc_instr = 
  | PStore of pc_expr * pc_lval
  (** Store: expression to store * pointer into which store *)
  
  | PCall of string * (pc_expr list) 
  (** Function call: function name * argument list *)
  
  | PIf of pc_expr * (pc_instr list) * (pc_instr list) 
  (** If: condition * true instruction list * false instruction list *)
  
  | PLabel of pc_label (** Label: name of label to write *)
  | PWhile of pc_instr list * (pc_label) 
  (** While loop : instr of loop * break label *)

  | PReturn of pc_expr (** Return: return expression *)
  | PDecl of pc_expr * pc_ptr 
  (** Variable declaration: expression to assign * pointer to assign *)
  
  | PCopy of pc_expr * pc_ptr (** Copy: expression to copy * pointer to copy to *)
  | PPop (** Pop operation *)
  | PRetAttr of pc_lval 
  (** Get return value: pointer into which store, comes after a PCall *)
  
  | PGoto of pc_label (** Goto operation: label to go *)
  | PAwaitInit (** Await global variables initialization is finished *)
  | PInitDone (** Global variables initialization is finished *)
  | PSkip (** Skip *)
  | PInitArray of int * pc_ptr 
  (** Init array: size of array * pointer to array *)

(** Procedure *)
type pc_procedure = { 
  pc_procedure_name: string; (** Name of the procedure *)
  pc_procedure_args: pc_var list; (** Argument names of the procedure *)
  pc_procedure_vars: pc_var list; (** Local variables of the procedure *)
  pc_procedure_body: pc_instr list; (*** Procedure instructions *)
}

(** Process *)
type pc_process = { 
  pc_process_name: string; (** Process name *)
  pc_process_set: string; 
  (** Set in which process is taken 
      e.g. process core \in CORES -> pc_process_set = CORES *)
  pc_process_vars: pc_var list; (** Local variables of process *)
  pc_process_body: pc_instr list; (** Process instructions *)
}

(** Program *)
type pc_prog = { 
  pc_prog_name: string; (** Program name *)
  pc_constants: (pc_var * int) list; (** Constants of the program *)

  pc_glob_var: ((pc_var * int option) * pc_expr) list; 
  (** Global program variables with their names and array sizes,
      if applicable, along with their declarations *)

  pc_nb_process: int; (** Number of processes *)
  pc_processus: pc_process list; (** List of processes *)
  pc_entry_point: string; (** Entry point of the program *)
  pc_procedures: pc_procedure list; (** List of procedures *)
}