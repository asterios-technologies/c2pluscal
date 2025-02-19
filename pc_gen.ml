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
  | CEnum e -> Result.ok (PEnumItem e.einame)
  | _ -> Result.error "Cst not treated"
  (* | CWStr of int64 list *)
  (* | CReal of float * fkind * string option *)

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
  | Lval lval -> Result.bind (pc_of_lval lval) (fun pc_lval -> Result.ok (PLval(pc_lval)))
  | UnOp (u,e, _) -> Result.bind (pc_of_unop u) (fun pc_unop ->
                                  Result.map (fun pc_exp -> PUnop(pc_unop,pc_exp)) (pc_of_exp e.enode))
  | BinOp (b, e1, e2, _) -> Result.bind (pc_of_binop b) (fun pc_binop ->
                                                Result.bind (pc_of_exp e1.enode) (fun pc_exp1 ->
                                                  Result.map (fun pc_exp2 -> PBinop(pc_binop,pc_exp1,pc_exp2)) (pc_of_exp e2.enode)))
  | AddrOf lval -> (match fst lval with
                          |Var vinfo -> Result.ok (PAddr(vinfo.vorig_name,vinfo.vglob))
                          |Mem _ -> Result.error "AddrOf Mem not treated")
  | SizeOf _ -> Result.ok (PCst (PInt 1))
  | CastE(_,e) -> pc_of_exp e.enode
  | StartOf lval -> (match fst lval with
                          |Var vinfo -> Result.ok (PAddr(vinfo.vorig_name,vinfo.vglob))
                          |Mem _ -> Result.error "StartOf Mem not treated")
  | e -> Result.error (Printf.sprintf "Exp not treated %s" (exp_to_str e))
  (* | SizeOfE e -> ()
  | SizeOfStr s -> ()
  | AlignOf t -> ()
  | AlignOfE e -> () *)

(*Converts a Lval into a PlusCal Lval*)
and pc_of_lval l =
  let mem_translate (e: exp) = let pc_exp_mem = pc_of_exp e.enode in
    (match pc_exp_mem with
        |Ok PLval(PLVar(ptr_info)) -> Result.ok (PLoad(PLVar(ptr_info)))
        |Ok PLval(PLoad(pc_lval)) -> Result.ok (PLoad(PLoad(pc_lval)))
        |Ok PLval(PField(field_info)) -> Result.ok (PField(field_info))
        |Ok PLval(PIndex(idx_info)) -> Result.ok (PIndex(idx_info))
        |Ok PBinop(PAddPI,PLval(PLVar(ptr_info)),e2) |Ok PBinop(PSubPI,PLval(PLVar(ptr_info)),e2)
          -> let pc_exp = add_pc_cst e2 1 in Result.ok (PIndex(pc_exp,PLVar(ptr_info)))
        |_ -> Result.error "Lval Mem access should be ptr\n")
  in

  match snd l with
  |NoOffset ->
    (match fst l with
        |Var vinfo -> Result.ok (PLVar((vinfo.vorig_name,vinfo.vglob)))
           (* (match vinfo.vtype with |TPtr _ -> Result.ok (PLPtr(vinfo.vorig_name,vinfo.vglob)) |_ -> Result.ok (PLVar((vinfo.vorig_name,vinfo.vglob)))) *)
        |Mem e -> mem_translate e)
  |Field(finfo,_) ->
    (match fst l with
      |Var vinfo -> Result.ok (PField(finfo.fname,PLVar(vinfo.vorig_name,vinfo.vglob)))
      |Mem e -> Result.bind (mem_translate e) (fun pc_lval ->
                      Result.ok (PField(finfo.fname,pc_lval))))
  |Index(e,_) -> Result.bind (pc_of_exp e.enode) (fun pc_exp ->
    let pc_exp = add_pc_cst pc_exp 1 in
    (match fst l with
    |Var vinfo -> Result.ok (PIndex(pc_exp,PLVar(vinfo.vorig_name,vinfo.vglob)))
    |Mem e -> Result.bind (mem_translate e) (fun pc_lval ->
                Result.ok (PIndex(pc_exp,pc_lval)))))

let rec init_to_pc_expr (i: init option) =
  match i with
    |None -> PUndef
    |Some init -> match init with
                    |CompoundInit _ -> complex_type_to_pc_expr init
                    |SingleInit e -> match pc_of_exp e.enode with
                                      |Error _ -> PUndef
                                      |Ok ok_exp -> ok_exp

and complex_type_to_pc_expr (i: init) =
  match i with
  |CompoundInit (_,l) ->
    (match List.length l with
      |0 -> PUndef |_ ->
      (match List.hd l with
        |(Field(_),_) -> let record_result =
                          fold_left_result
                          (fun (offs,i) -> (match offs with
                            |Field(f,_) -> Result.ok (f.fname,init_to_pc_expr (Some i))
                            |Index(_) -> Result.error "Index offset should not appear in struct init"
                            |_ -> Result.error "Offset should be Field or Index"))
                          (fun acc i -> acc@[i]) [] l
                        in (match record_result with
                              |Error _ -> PUndef
                              |Ok ok_record -> PCst(PRecord(ok_record)))
        |(Index(_),_) -> let array_result =
                          fold_left_result
                          (fun (offs,i) -> (match offs with
                            |Index(e,_) -> Result.bind (pc_of_exp e.enode) (fun pc_e ->
                                            Result.ok (pc_e,init_to_pc_expr (Some i)))
                            |Field(_) -> Result.error "Field offset should not appear in array init"
                            |_ -> Result.error "Offset should be Field or Index"))
                          (fun acc i -> acc@[i]) [] l
                        in (match array_result with
                              |Error _ -> PUndef
                              |Ok ok_array -> PCst(PArray(ok_array)))
        |_ -> PUndef))
  |_ -> PUndef

(* Return Error if an error occurs in one exp of the list, OK(exp_list) , with exp_list list of pc_expr *)
let pc_of_exp_list l = fold_left_result (fun e -> pc_of_exp e.enode) (fun acc e -> acc@[e]) [] l

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
            |Ok PLval(PLVar(fname,_)) ->
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
                                                |Ok ok_exp -> Result.ok ([PStore(ok_exp,PLVar(vinfo.vorig_name,vinfo.vglob))])
                                                  (* (match vinfo.vtype with
                                                  |TPtr _ -> Result.ok ([PStore(ok_exp,PLPtr(vinfo.vorig_name,vinfo.vglob))])
                                                  |_ -> Result.ok ([PStore(ok_exp,PLVar(vinfo.vorig_name,vinfo.vglob))])) *)
                                                |Error e -> Error e)
                            |CompoundInit _ -> let record = complex_type_to_pc_expr init in
                                                 Result.ok ([PStore(record,PLVar(vinfo.vorig_name,vinfo.vglob))]))
      |ConsInit (finfo, args, _) -> (match pc_of_exp_list args with
                                      |Error e -> Error e
                                      |Ok pc_args_list ->
                                        Result.ok [PCall(finfo.vorig_name,pc_args_list);
                                                  PRetAttr(PLVar(vinfo.vorig_name,vinfo.vglob))]
                                        (* (match vinfo.vtype with
                                          |TPtr _ -> Result.ok [PCall(finfo.vorig_name,pc_args_list);
                                                                PRetAttr(PLPtr(vinfo.vorig_name,vinfo.vglob))]
                                          |_ -> Result.ok [PCall(finfo.vorig_name,pc_args_list);
                                                           PRetAttr(PLVar(vinfo.vorig_name,vinfo.vglob))]) *)
                                    ))
  | Skip _ -> Result.ok ([])
  | i -> Result.error (Printf.sprintf "Instr not treated %s" (instr_to_str i))
  (* | Asm of attributes * string list * extended_asm option * location
  | Code_annot of code_annotation * location *)

let rec pc_of_stmt = function
  | Instr i -> (pc_of_instr i, 0)
  | Return (r,_) -> (match r with
                                    |Some e -> let pc_exp = pc_of_exp e.enode in
                                                (match pc_exp with
                                                  |Ok ok_exp -> (Result.ok [PReturn(ok_exp)], 0)
                                                  |Error e -> (Error e, 0))
                                    |None -> (Result.ok [], 0))
  | Goto (r,_) -> (Result.ok [PGoto(Pretty_utils.to_string Printer.pp_label (List.hd (!r).labels))], 0)
  | If (e,b1,b2,_) -> (match pc_of_exp e.enode with
                                                    |Error err -> (Error err, 0)
                                                    |Ok ok_exp -> match pc_of_block b1.bstmts with
                                                                   |Error err -> (Error err, 0)
                                                                   |Ok ok_b1 -> match pc_of_block b2.bstmts with
                                                                                |Error err -> (Error err, 0)
                                                                                |Ok ok_b2 -> (Result.ok [PIf(ok_exp, ok_b1, ok_b2)],
                                                                                             List.length b1.bstmts + List.length b2.bstmts))
  | Loop (_,b,_,_,s) ->
      (match s with |None -> Result.error "Break stmts in Loop should appear", 0
      |Some(s) ->
        Result.bind (pc_of_block b.bstmts) (fun pc_block ->
          Result.ok ([PWhile(pc_block,Pretty_utils.to_string Printer.pp_label (List.hd s.labels))])), List.length b.bstmts+1)
  | Break loc -> Result.ok ([PGoto(Pretty_utils.to_string Printer.pp_location loc)]), 0
  | Block b -> pc_of_block b.bstmts, List.length b.bstmts
  | UnspecifiedSequence l ->
    let block = (Cil.block_from_unspecified_sequence l).bstmts in pc_of_block block, List.length block

  | s -> Result.error (Printf.sprintf "Stmt not treated %s" (stmt_to_str s)), 0
  (* | TryFinally _ | TryExcept _ | TryCatch _ -> Format.fprintf out "try : "; Format.fprintf out "end try \n";
  | Throw _ -> Format.fprintf out "throw : "; Format.fprintf out "end throw \n";
  | Continue l -> Format.pp_print_string out "continue :"; Printer.pp_location out l; Format.fprintf out "end continue \n";
  | Switch(e,b,stmt_list,loc) -> Format.fprintf out " switch : "; Printer.pp_exp out e; Printer.pp_block out b; List.iter (fun s -> Printer.pp_stmt out s) stmt_list; Printer.pp_location out loc; Format.fprintf out "end switch \n";*)

and pc_of_block b =
  if List.length b = 0 then Result.ok ([PSkip])
  else
    fold_left_result (fun s -> fst (pc_of_stmt s.skind)) (fun acc s_list -> acc@s_list) [] b

let pc_constant_of_enum (e: enumitem) = match e.eival.enode with
    | Const(CInt64(i,_,_)) -> Result.ok (e.einame, Integer.to_int_exn i)
    | _ -> Result.error "Enum item should have cst value"


let rec procedure_push_vars acc proc_name (vars: (pc_var * int option) list) (is_args: bool) =
  match vars with
    |[] -> acc
    |(vname, Some array_size)::q -> if is_args then procedure_push_vars (PCopy((vname,false), (vname_to_string proc_name vname,false))::acc) proc_name q is_args
                                    else procedure_push_vars (PDecl(PUndef, (vname,false))::(PInitArray(array_size,(vname,false))::acc)) proc_name q is_args
    |(vname, None)::q -> if is_args then procedure_push_vars (PDecl(PArg((vname)), (vname,false))::acc) proc_name q is_args
                         else procedure_push_vars (PDecl(PUndef, (vname,false))::acc) proc_name q is_args

let rec procedure_pop acc n =
  match n with
    |0 -> acc
    |_ -> procedure_pop (PPop::acc) (n-1)

class gen_pc (prog: pc_prog ref) = object
  inherit Visitor.frama_c_inplace

  val child_to_skip = ref 0;

  method! vfile f =
    let entry_point = List.hd (List.rev
      (List.filter_map (fun g -> match g with |GFun(fundec,_) -> Some fundec.svar.vorig_name |_ -> None) f.globals)) in
    let name = get_file_name() in
    let nb_process =  1 in
    let processus = [{pc_process_name="proc";
                      pc_process_set="PROCESS";
                      pc_process_vars=[];
                      pc_process_body=[PAwaitInit;PCall(entry_point,[])]}]
    in
      prog := {!prog with
               pc_prog_name = name;
               pc_entry_point = entry_point;
               pc_nb_process = nb_process;
               pc_processus = processus};
      Cil.DoChildrenPost(fun f ->
        let glob_init_process =
          {pc_process_name="globalInit";
          pc_process_set="GLOBAL_INIT";
          pc_process_vars=[];
          pc_process_body=(List.fold_left (fun acc ((name, int_opt),expr) ->
            match int_opt with
              |Some array_size -> (PDecl(PUndef, (name,true))::(PInitArray(array_size,(name,true))::acc))
              |None -> (PDecl(expr, (name,true))::acc)) [] (!prog).pc_glob_var)@[PInitDone]}
        in
        prog := {!prog with
                  pc_processus = glob_init_process::(!prog).pc_processus};f)

  method! vglob_aux g =
    match g with
      | GVar(varinfo, initinfo, _) -> prog := {!prog with
                                          pc_glob_var = (varinfo_to_pc_decl varinfo,init_to_pc_expr initinfo.init)
                                                        ::(!prog).pc_glob_var;};
                                      Cil.DoChildren
      | GCompTag(compinfo, _) -> if compinfo.cstruct then Cil.DoChildren
                                 else (Printf.eprintf "Union not supported"; Cil.DoChildren)
      | GEnumTag (enuminfo,_) -> (match fold_left_result (fun e -> pc_constant_of_enum e) (fun acc e -> acc@[e]) [] enuminfo.eitems with
                                  |Error e -> Printf.eprintf "Error : %s" e; Cil.DoChildren
                                  |Ok enum_items ->
                                    prog := {!prog with
                                      pc_constants = (!prog).pc_constants@enum_items};
                                    Cil.DoChildren)
      | GFun(fundec, _) ->  Cfg.prepareCFG fundec;
                            let proc_name = fundec.svar.vorig_name in
                            let args = List.map (varinfo_to_pc_decl) fundec.sformals in
                             let vars = List.map (varinfo_to_pc_decl) fundec.slocals in
                             let args_decl = procedure_push_vars [] proc_name args true in
                             let vars_decl = procedure_push_vars [] proc_name vars false in
                             let pop_list = procedure_pop [] (List.length args_decl + List.length vars_decl) in
                              prog := {!prog with
                                      pc_procedures = {pc_procedure_name=proc_name;
                                                       pc_procedure_args=List.map (varinfo_to_pcvar) fundec.sformals;
                                                       pc_procedure_vars=List.map (varinfo_to_pcvar) fundec.slocals;
                                                       pc_procedure_body=args_decl@vars_decl}
                                                      ::(!prog).pc_procedures;};
                             Cil.DoChildrenPost(fun g ->
                              match (!prog).pc_procedures with
                              |curr_proc::q ->
                                let block = if Options.BlockMain.get() && curr_proc.pc_procedure_name = (!prog).pc_entry_point
                                  then [PBlock] else []
                                in
                                prog :={!prog with
                                  pc_procedures = {curr_proc with
                                                  pc_procedure_body=
                                                  curr_proc.pc_procedure_body@
                                                  block@
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
      | GEnumTagDecl _ -> Format.fprintf out "GEnumTagDecl : "; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GEnumTagDecl \n"; g)
      | GVarDecl(varinfo, loc) -> Format.fprintf out "GVarDecl : "; Printer.pp_varinfo out varinfo; Printer.pp_location out loc; Cil.DoChildrenPost (fun g -> Format.fprintf out "End GVarDecl \n"; g)
      | GFunDecl(funspec, varinfo, loc) -> Format.fprintf out "GFunDecl : "; Printer.pp_funspec out funspec ; Printer.pp_varinfo out varinfo; Printer.pp_location out loc;
                      Cil.DoChildrenPost (fun g -> Format.fprintf out "End GFunDecl \n"; g) *)

  method! vstmt_aux s =
    match !child_to_skip with |i when i > 0 ->
      child_to_skip := !child_to_skip - 1;
      Cil.SkipChildren
    |_ ->
    let (pc_instrs_result,nb_skip) = pc_of_stmt s.skind in
    child_to_skip := nb_skip;
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
                                            Cil.DoChildren
                                            |lbl::_ -> prog :=
                                                      {!prog with
                                                      pc_procedures = {curr_proc with
                                                                        pc_procedure_body=
                                                                        curr_proc.pc_procedure_body@
                                                                        (PLabel(Pretty_utils.to_string Printer.pp_label lbl)
                                                                        ::pc_instrs)}
                                                                      ::q;};
                                            Cil.DoChildren)
                        |[] -> Printf.eprintf "Error no procedure"; Cil.DoChildren
end