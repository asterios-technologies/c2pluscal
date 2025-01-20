open Cil_types
open Pc
open Pc_utils

let pc_of_unop = function
  | Neg -> Result.ok PMinus
  | LNot -> Result.ok PNot
  | BNot -> Result.error "Unop not treated"

let pc_of_binop = function
  | PlusA -> Result.ok PAdd
  | PlusPI -> Result.ok PAddPI
  | MinusA -> Result.ok PSub
  | MinusPI -> Result.ok PSubPI
  | MinusPP -> Result.ok PSubPP
  | Mult -> Result.ok PMul
  | Div -> Result.ok PDiv
  | Mod -> Result.ok PMod
  | Lt -> Result.ok PLt
  | Gt -> Result.ok PGt
  | Le -> Result.ok PLe
  | Ge -> Result.ok PGe
  | Eq -> Result.ok PEq
  | Ne -> Result.ok PNe
  | LAnd -> Result.ok PLand
  | LOr -> Result.ok PLor
  | _ -> Result.error "Binop not treated"
  (* | BAnd
  | BXor
  | BOr
  | Shiftlt -> Result.ok PShiftL
  | Shiftrt -> Result.ok PShiftR *)

let pc_of_cst = function
  | CInt64 (i,_,_) -> Result.ok (PInt(Integer.to_int_exn i))
  | CStr s -> Result.ok (PString s)
  | CChr c -> Result.ok (PString (String.make 1 c))
  | _ -> Result.error "Cst not treated"
  (* | CWStr of int64 list *)
  (* | CReal of float * fkind * string option *)
  (* | CEnum of enumitem *)

let exp_to_str = function
  | SizeOfE _ -> "SizeOfE"
  | SizeOfStr _ -> "SizeOfStr"
  | AlignOf _ -> "AlignOf"
  | AlignOfE _ -> "AlignOfE"
  | CastE _ -> "CastE"
  | StartOf _ -> "StartOf"
  |_ -> ""

let instr_to_str = function
  | Asm _ -> "Asm"
  | Skip _ -> "Skip"
  | Code_annot _ -> "Code Annot"
  |_ -> ""

let stmt_to_str = function
  | TryFinally _ | TryExcept _ | TryCatch _ -> "Try"
  | Throw _ -> "Throw"
  | Break _ -> "Break"
  | Continue _ -> "Continue"
  | Switch _ -> "Switch"
  | Loop _ -> "Loop"
  | UnspecifiedSequence _ -> "UnspecifiedSequence"
  | _ -> ""

let rec pc_of_exp = function
  | Const c -> Result.map (fun pc_cst -> PCst pc_cst) (pc_of_cst c)
  | Lval lval -> Result.bind (pc_of_lval lval) (fun pc_lval -> Result.ok (PLoad(pc_lval)))
  | UnOp (u,e, _) -> Result.bind (pc_of_unop u) (fun pc_unop ->
                                  Result.map (fun pc_exp -> PUnop(pc_unop,pc_exp)) (pc_of_exp e.enode))
  | BinOp (b, e1, e2, _) -> Result.bind (pc_of_binop b) (fun pc_binop ->
                                                    Result.bind (pc_of_exp e1.enode) (fun pc_exp1 ->
                                                      Result.map (fun pc_exp2 -> PBinop(pc_binop,pc_exp1,pc_exp2)) (pc_of_exp e2.enode)))
  | AddrOf lval -> (match fst lval with
                          |Var vinfo -> Result.ok (PLoad(PLPtr(vinfo.vorig_name,vinfo.vglob)))
                          |Mem e -> pc_of_exp e.enode)
  | SizeOf _ -> Result.ok (PCst (PInt 1))
  | CastE(_,e) -> pc_of_exp e.enode
  | e -> Result.error (Printf.sprintf "Exp not treated %s" (exp_to_str e))
  (* | SizeOfE e -> ()
  | SizeOfStr s -> ()
  | AlignOf t -> ()
  | AlignOfE e -> ()
  | StartOf lval -> () *)

(*Converts a Lval into a PlusCal Lval*)
and pc_of_lval l =
  match snd l with
  |NoOffset ->
    (match fst l with
        |Var vinfo -> (match vinfo.vtype with |TPtr _ -> Result.ok (PLPtr(vinfo.vorig_name,vinfo.vglob)) |_ -> Result.ok (PLVar((vinfo.vorig_name,vinfo.vglob))))
        |Mem e -> let pc_exp_mem = pc_of_exp e.enode in
                        (match pc_exp_mem with
                            |Ok PLoad(PLPtr(ptr_info)) -> Result.ok (PLVar(ptr_info))
                            |_ -> Result.error "Lval Mem access should be ptr\n"))
  |Field(finfo,_) ->
    (match fst l with
      |Var vinfo -> Result.ok (PField(finfo.fname,(vinfo.vorig_name,vinfo.vglob)))
      |Mem e -> let pc_exp_mem = pc_of_exp e.enode in
                      (match pc_exp_mem with
                          |Ok PLoad(PLPtr(ptr_info)) -> Result.ok (PField(finfo.fname, ptr_info))
                          |_ -> Result.error "Lval Mem access should be ptr\n"))
  |Index _ -> Result.error "Index offset not treated"

let rec init_to_pc_expr (i: init option) =
  match i with
    |None -> PUndef
    |Some init -> match init with
                    |CompoundInit _ -> struct_to_pc_expr init
                    |SingleInit e -> match pc_of_exp e.enode with
                                      |Error _ -> PUndef
                                      |Ok ok_exp -> ok_exp

and struct_to_pc_expr (i: init) =
  match i with
  |CompoundInit (_,l) ->
      let record_result =
        fold_left_result l (fun (offs,i) ->
        (match offs with
          |Field(f,_) -> Result.ok (f.fname,init_to_pc_expr (Some i))
          |Index(_) -> Result.error "Index offset not treated"
          |_ -> Result.error "Offset should be Field or Index"))
      in (match record_result with
            |Error _ -> PUndef
            |Ok ok_record -> PCst(PRecord(ok_record)))
  |_ -> PUndef

(* Return Error if an error occurs in one exp of the list, OK(exp_list) , with exp_list list of pc_expr *)
let pc_of_exp_list l = fold_left_result l (fun e -> pc_of_exp e.enode)

let pc_of_instr = function
  | Set(lval, e, _) -> Result.bind (pc_of_exp e.enode) (fun pc_exp ->
                                    Result.bind (pc_of_lval lval) (fun pc_lval ->
                                      Result.ok ([PStore(pc_exp, pc_lval)])))
  | Call(lval_opt, e, e_list, _) ->
    (match pc_of_exp_list e_list with
      |Error e -> Error e
      |Ok pc_exp_list ->
        let pc_exp = pc_of_exp e.enode in
          (match pc_exp with
            |Ok PLoad(PLVar(fname,_)) ->
              (match lval_opt with
                |Some lval -> Result.bind (pc_of_lval lval) (fun pc_lval ->
                                Result.ok ([PCall(fname, pc_exp_list);
                                            PRetAttr((pc_lval))]))
                |None -> Result.ok ([PCall(fname, pc_exp_list)]))
            |Ok _ -> Error "Call instr should have PLoad(PLVar()) exp"
            |Error e -> Error e))
  | Local_init (vinfo, local_init, _) ->
    (match local_init with
      |AssignInit init -> (match init with
                            |SingleInit e -> let pc_exp = pc_of_exp e.enode in
                                              (match pc_exp with
                                                |Ok ok_exp ->
                                                  (match vinfo.vtype with
                                                  |TPtr _ -> Result.ok ([PStore(ok_exp,PLPtr(vinfo.vorig_name,vinfo.vglob))])
                                                  |_ -> Result.ok ([PStore(ok_exp,PLVar(vinfo.vorig_name,vinfo.vglob))]))
                                                |Error e -> Error e)
                            |CompoundInit _ -> let record = struct_to_pc_expr init in
                                                 Result.ok ([PStore(record,PLVar(vinfo.vorig_name,vinfo.vglob))]))
      |ConsInit (finfo, args, _) -> (match pc_of_exp_list args with
                                      |Error e -> Error e
                                      |Ok pc_args_list ->
                                        (match vinfo.vtype with
                                          |TPtr _ -> Result.ok [PCall(finfo.vorig_name,pc_args_list);
                                                                PRetAttr(PLPtr(vinfo.vorig_name,vinfo.vglob))]
                                          |_ -> Result.ok [PCall(finfo.vorig_name,pc_args_list);
                                                           PRetAttr(PLVar(vinfo.vorig_name,vinfo.vglob))])))
  | Skip _ -> Result.ok ([])
  | i -> Result.error (Printf.sprintf "Instr not treated %s" (instr_to_str i))
  (* | Asm of attributes * string list * extended_asm option * location
  | Code_annot of code_annotation * location *)

let rec pc_of_stmt = function
  | Instr i -> pc_of_instr i
  | Return (r,_) -> (match r with
                                    |Some e -> let pc_exp = pc_of_exp e.enode in
                                                (match pc_exp with
                                                  |Ok ok_exp -> Result.ok [PReturn(ok_exp)]
                                                  |Error e -> Error e)
                                    |None -> Result.ok [])
  | Goto (r,_) -> Result.ok [PGoto(Pretty_utils.to_string Printer.pp_label (List.hd (!r).labels))]
  | If (e,b1,b2,_) -> (match pc_of_exp e.enode with
                                                    |Error err -> Error err
                                                    |Ok ok_exp -> match pc_of_block b1.bstmts with
                                                                   |Error err -> Error err
                                                                   |Ok ok_b1 -> match pc_of_block b2.bstmts with
                                                                                |Error err -> Error err
                                                                                |Ok ok_b2 -> Result.ok [PIf(ok_exp, ok_b1, ok_b2)])
  (* | Loop (l,b,loc,stmt1,stmt2) -> Result.error "Stmt not treated" *)
  | Block b -> pc_of_block b.bstmts
  | UnspecifiedSequence _ -> Result.ok ([])
  | s -> Result.error (Printf.sprintf "Stmt not treated %s" (stmt_to_str s))
  (* | TryFinally _ | TryExcept _ | TryCatch _ -> Format.fprintf out "try : "; Format.fprintf out "end try \n";
  | Throw _ -> Format.fprintf out "throw : "; Format.fprintf out "end throw \n";
  | Break l -> Format.pp_print_string out "break : "; Printer.pp_location out l; Format.fprintf out "end break \n";
  | Continue l -> Format.pp_print_string out "continue :"; Printer.pp_location out l; Format.fprintf out "end continue \n";
  | Switch(e,b,stmt_list,loc) -> Format.fprintf out " switch : "; Printer.pp_exp out e; Printer.pp_block out b; List.iter (fun s -> Printer.pp_stmt out s) stmt_list; Printer.pp_location out loc; Format.fprintf out "end switch \n";*)

and pc_of_block b =
  List.fold_left (fun acc stmt ->
    Result.bind acc (fun ok_acc ->
        Result.bind (pc_of_stmt stmt.skind)
          (fun ok_stmt ->
          Result.ok (ok_acc@ok_stmt))))
  (Result.ok []) b

let rec procedure_push_vars acc (vars: pc_var list) (is_args: bool) =
  match vars with
    |[] -> acc
    |(vname,bool_ptr)::q -> if bool_ptr then procedure_push_vars acc q is_args
                              else if is_args then procedure_push_vars (PDecl(PArg((vname,bool_ptr)), (vname,false))::acc) q is_args
                                   else procedure_push_vars (PDecl(PUndef, (vname,false))::acc) q is_args

let rec procedure_pop acc n =
  match n with
    |0 -> acc
    |_ -> procedure_pop (PPop::acc) (n-1)

class gen_pc (prog: pc_prog ref) = object
  inherit Visitor.frama_c_inplace

  method! vfile _ =
    let name = get_file_name() in
    let nb_process =  1 in
    let processus = [{pc_process_name="proc";
                      pc_process_set="PROCESS";
                      pc_process_vars=[];
                      pc_process_body=[PAwaitInit;PCall("main",[])]}]
    in
      prog := {!prog with
               pc_prog_name = name;
               pc_nb_process = nb_process;
               pc_processus = processus};
      Cil.DoChildrenPost(fun f ->
        let glob_init_process =
          {pc_process_name="globalInit";
          pc_process_set="GLOBAL_INIT";
          pc_process_vars=[];
          pc_process_body=(List.fold_left (fun acc ((name,_),expr) ->
            (PDecl(expr, (name,true))::acc)) [] (!prog).pc_glob_var)@[PInitDone]}
        in
        prog := {!prog with
                  pc_processus = glob_init_process::(!prog).pc_processus};f)

  method! vglob_aux g =
    match g with
      | GVar(varinfo, initinfo, _) -> prog := {!prog with
                                          pc_glob_var = (varinfo_to_pcvar varinfo,init_to_pc_expr initinfo.init)
                                                        ::(!prog).pc_glob_var;};
                                      Cil.DoChildren
      | GCompTag(compinfo, _) -> if compinfo.cstruct then Cil.DoChildren
                                 else (Printf.eprintf "Union not supported"; Cil.DoChildren)
      | GFun(fundec, _) -> let args = List.map (varinfo_to_pcvar) fundec.sformals in
                             let vars = List.map (varinfo_to_pcvar) fundec.slocals in
                             let args_decl = procedure_push_vars [] args true in
                             let vars_decl = procedure_push_vars [] vars false in
                             let pop_list = procedure_pop [] (List.length args_decl + List.length vars_decl) in
                              prog := {!prog with
                                      pc_procedures = {pc_procedure_name=fundec.svar.vorig_name;
                                                       pc_procedure_args=args;
                                                       pc_procedure_vars=vars;
                                                       pc_procedure_body=args_decl@vars_decl}
                                                      ::(!prog).pc_procedures;};
                             Cil.DoChildrenPost(fun g ->
                              match (!prog).pc_procedures with
                              |curr_proc::q -> prog :=
                                {!prog with
                                pc_procedures = {curr_proc with
                                                  pc_procedure_body=
                                                  curr_proc.pc_procedure_body@
                                                  pop_list}
                                                ::q;}; g
                              |[] -> Printf.eprintf "Error no procedure"; g)
      | _ -> Cil.DoChildren
      (* | GAsm _ -> Format.fprintf out "GAsm : "; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GAsm \n"; g)
      | GPragma _ -> Format.fprintf out "GPragma : "; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GPragma \n"; g)
      | GText _ -> Format.fprintf out "GText : "; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GText \n"; g)
      | GAnnot _ -> Format.fprintf out "GAnnot : "; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GAnnot \n"; g)
      | GType(typeinfo, loc) -> Format.fprintf out "GType : "; Format.pp_print_string out typeinfo.torig_name; Format.pp_print_string out typeinfo.tname ; Printer.pp_typ out typeinfo.ttype; Format.pp_print_bool out typeinfo.treferenced; Printer.pp_location out loc;
        Cil.DoChildrenPost (fun g -> Format.fprintf out "End GType \n"; g)
      | GCompTagDecl(compinfo, loc) -> Format.fprintf out "GCompTagDecl : "; Printer.pp_compinfo out compinfo; Printer.pp_location out loc; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GCompTagDecl \n"; g)
      | GEnumTag _ -> Format.fprintf out "GEnumTag : "; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GEnumTag \n"; g)
      | GEnumTagDecl _ -> Format.fprintf out "GEnumTagDecl : "; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GEnumTagDecl \n"; g)
      | GVarDecl(varinfo, loc) -> Format.fprintf out "GVarDecl : "; Printer.pp_varinfo out varinfo; Printer.pp_location out loc; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GVarDecl \n"; g)
      | GFunDecl(funspec, varinfo, loc) -> Format.fprintf out "GFunDecl : "; Printer.pp_funspec out funspec ; Printer.pp_varinfo out varinfo; Printer.pp_location out loc;
                      Cil.DoChildrenPost (fun g -> Format.fprintf out "End GFunDecl \n"; g) *)

  method! vstmt_aux s =
    let skip = skip_children s.skind in
    match skip with |1 -> Cil.SkipChildren |_ ->
    let next = (match skip with |2-> Cil.SkipChildren |_ -> Cil.DoChildren) in
    let pc_instrs_result = pc_of_stmt s.skind in
    match pc_instrs_result with
      |Error e -> Printf.eprintf "Error : %s" e; Cil.DoChildren
      |Ok pc_instrs -> match (!prog).pc_procedures with
                        |curr_proc::q -> (match s.labels with
                                            |[] -> prog :=
                                                    {!prog with
                                                    pc_procedures = {curr_proc with
                                                                      pc_procedure_body=
                                                                      curr_proc.pc_procedure_body@
                                                                      pc_instrs}
                                                                    ::q;};
                                            next
                                            |lbl::_ -> prog :=
                                                      {!prog with
                                                      pc_procedures = {curr_proc with
                                                                        pc_procedure_body=
                                                                        curr_proc.pc_procedure_body@
                                                                        (PLabel(Pretty_utils.to_string Printer.pp_label lbl)
                                                                        ::pc_instrs)}
                                                                      ::q;};
                                            next)
                        |[] -> Printf.eprintf "Error no procedure"; Cil.DoChildren
end