type pc_label = string (*Label of a line*)
type pc_ptr = (string * bool) (*Ptr : ptr name * is_ptr, eg. bool flag telling if the ptr is global or not*)
type pc_var = (string * bool) (*Var : var name * is_ptr, eg. bool flag telling if the var is a ptr in C code or not*)

(*BINOP*)
type pc_binop = PAdd | PSub | PMul | PDiv | PMod
              | PAddPI | PSubPI | PSubPP
              | PLt | PGt | PLe | PGe | PEq | PNe
              | PLand | PLor

(*UNOP*)
type pc_unop = PMinus | PNot

(*CST*)
type pc_cst = PInt of int
              |PString of string
              |PRecord of (string * pc_expr) list (*Record type : [a |-> ..., b |-> ..., etc]*)

(*LVALUE*)
and pc_lval = |PLVar of pc_ptr
              |PLPtr of pc_ptr
              |PField of (string * pc_ptr) (*Field of a record : name of field * name of ptr*)

(*EXPR*)
and pc_expr = PCst of pc_cst (*constant value : cst*)
              |PLoad of pc_lval (*load a ptr : ptr to load*)
              |PArg of pc_var (*arg of func : arg name*)
              |PBinop of pc_binop * pc_expr * pc_expr (*binop : binop * fst op * snd op*)
              |PUnop of pc_unop * pc_expr (*unop : unop * fst op*)
              |PUndef (*undef for empty decl*)

(*TYPE FOR TYPEOK*)
type pc_type = PStruct of string * (string list) (*struct : struct name * (field name list) *)
               |PEnum

(*INSTR*)
type pc_instr = PStore of pc_expr * pc_lval (*store : expr to store * ptr to store*)
                |PCall of string * (pc_expr list) (*func call : func name * arg list*)
                |PIf of pc_expr * (pc_instr list) * (pc_instr list) (*if : cond * true instr list * false instr list*)
                |PLabel of pc_label (*label : name of label to write*)
                (*loop: block of instr of the loop * (continue instr * label) * (break instr * label)*)
                |PWhile of pc_instr list * (pc_label)
                |PReturn of pc_expr (*return : return expr*)
                |PDecl of pc_expr * pc_ptr (*var decl : expr to assign * ptr to assign*)
                |PPop (*pop op*)
                |PRetAttr of pc_lval (*get return value : ptr to store, comes after a PCall*)
                |PGoto of pc_label (*goto op : label to go*)
                |PAwaitInit (*await init is finished flag*)
                |PInitDone (*init is finished flag*)
                |PSkip (*skip*)

(*PROCEDURE*)
type pc_procedure = { pc_procedure_name: string; (*name of procedure*)
                      pc_procedure_args: pc_var list; (*args name of procedure*)
                      pc_procedure_vars: pc_var list; (*local vars of procedure*)
                      pc_procedure_body: pc_instr list; (*procedure instrs*)
                    }

(*PROCESS*)
type pc_process = { pc_process_name: string; (*process name*)
                    pc_process_set: string; (*set in which process is taken, ex : process core \in CORES -> pc_process_set = CORES*)
                    pc_process_vars: pc_var list; (*local vars of process*)
                    pc_process_body: pc_instr list; (*process instrs*)
                  }

(*PROG*)
type pc_prog = { pc_prog_name: string; (*program name*)
                 pc_glob_var: (pc_var * pc_expr) list; (*prog glob vars with decl*)
                 pc_nb_process: int; (*nb of process*)
                 pc_processus: pc_process list; (*list of process*)
                 pc_procedures: pc_procedure list; (*list of procedures*)
               }

