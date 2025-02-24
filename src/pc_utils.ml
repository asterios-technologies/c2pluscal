open Cil_types
open Pc

let get_file_name() =
    let full_name =  Filepath.Normalized.to_pretty_string (List.hd (Kernel.Files.get())) in
    let base_name = Filename.basename full_name in
    String.sub base_name 0 (String.length base_name - 2)

let is_binop_ptr (b: pc_binop) = match b with
    | PAddPI | PSubPI | PSubPP -> true
    |_ -> false

let vname_to_string (proc_name: string) (vname: string) =
    let name_strings = [vname;"_ptr_";proc_name] in
    String.concat "" name_strings

let arg_to_string (proc_name: string) (vname: pc_var) =
    let name_strings = [vname;"_";proc_name] in
        String.concat "" name_strings

let ptr_to_string (proc_name: string) ((ptr_name,glob): pc_ptr) =
    let suffix = if glob then "glob" else proc_name in
    let name_strings = [ptr_name;"_ptr_";suffix] in
    String.concat "" name_strings

let varinfo_to_pcvar (v: varinfo) = (v.vorig_name)

let varinfo_to_pc_decl (v: varinfo) =
    match v.vtype with
        |TArray(_,e,_) -> (v.vorig_name, (Some (Cil.lenOfArray e)))
        |_ -> (v.vorig_name, None)

let rec dump_list out l dump_fun =
    match l with
        |[] -> Format.fprintf out ""
        |t::[] -> dump_fun out t
        |t::q -> dump_fun out t; Format.fprintf out ","; dump_list out q dump_fun

let string_of_indent n = (String.make (n * 4) ' ')
let add_indent n indent = String.concat "" [(string_of_indent n);indent]

let remove_last_char s =
    if String.length s = 0 then
      s
    else
      String.sub s 0 (String.length s - 1)

(*Fold f on l = [i1,i2,...,in], with Error if one
  of f ik = error, and ok(g(g (g init_acc (f i1)) i2) ...) otherwise*)
let fold_left_result f g init_acc l =
    List.fold_left (fun acc i ->
        Result.bind acc (fun ok_acc ->
            Result.bind (f i) (fun ok_i ->
                Result.ok (g ok_acc ok_i))))
    (Result.ok init_acc) l

let add_pc_cst (e: pc_expr) (n: int) =
    match e with
        |PCst(PInt(i)) -> PCst(PInt(i+n))
        |_ -> e

let get_entry_point (f: file) =
    List.hd (List.rev
        (List.filter_map (fun g ->
            match g with
                |GFun(fundec,_) -> Some fundec.svar.vorig_name
                |_ -> None)
        f.globals))

(*Detects all the StartOf op (i.e. array conversion to ptr) in functions calls in a stmts list
  and add the array args index in a hashtable with the function name as key

  @params
    fun_array_args_table : hashtable with function name as key and a list of args index as value
    stms : list of stmts to analyze*)
let detect_array_args (array_args_table: (string, int) Hashtbl.t)  (stmts: stmt list) =
    List.iter (fun s ->
        match s.skind with
            |Instr i -> (match i with
                |Call(_,e, args, _) -> (match e.enode with
                    |Lval(Var vinfo, NoOffset) ->
                        (List.iteri (fun i arg -> match arg.enode with
                            |StartOf(_) -> Hashtbl.add array_args_table vinfo.vorig_name i
                            |_ -> ()) args)
                    |_ -> ())
                |_ -> ())
            |_ -> ()) stmts

let get_all_array_args (array_args_table: (string, int) Hashtbl.t) (globals: global list) =
    List.iter (fun g ->
        match g with
            |GFun(fundec,_) -> (detect_array_args array_args_table fundec.sbody.bstmts)
            |_ -> ()) globals;