type pc_label = string
type pc_ptr = (string * bool)
type pc_var = (string * bool)

type pc_binop = PAdd | PSub | PMul | PDiv | PMod
              | PAddPI | PSubPI | PSubPP
              | PLt | PGt | PLe | PGe | PEq | PNe
              | PLand | PLor

type pc_unop = PMinus | PNot

type pc_cst = PInt of int
              |PString of string

type pc_expr = PCst of pc_cst
               |PLoad of pc_ptr
               |PArg of pc_var
               |PPtr of pc_ptr
               |PBinop of pc_binop * pc_expr * pc_expr
               |PUnop of pc_unop * pc_expr
               |PUndef


type pc_instr = PStore of pc_expr * pc_ptr
                     |PStorePtr of pc_expr * pc_ptr
                     |PCall of string * (pc_expr list)
                     |PIf of pc_expr * (pc_instr list) * (pc_instr list)
                     |PWhile
                     |PReturn of pc_expr
                     |PDecl of pc_expr * pc_ptr
                     |PPop
                     |PRetAttr of pc_ptr
                     |PGoto of pc_label
                     |PLabel of pc_label
                     |PAwaitInit
                     |PInitDone

type pc_procedure = { pc_procedure_name: string;
                      pc_procedure_args: pc_var list;
                      pc_procedure_vars: pc_var list;
                      pc_procedure_body: pc_instr list;
                    }

type pc_process = { pc_process_name: string;
                    pc_process_set: string;
                    pc_process_vars: pc_var list;
                    pc_process_body: pc_instr list;
                  }

type pc_prog = { pc_prog_name: string;
                 pc_glob_var: (pc_var * pc_expr) list;
                 pc_nb_process: int;
                 pc_processus: pc_process list;
                 pc_procedures: pc_procedure list;
               }

