open Cil_types
open Pc

let get_file_name() =
    let full_name =  Filepath.Normalized.to_pretty_string (List.hd (Kernel.Files.get())) in
    String.sub full_name 0 (String.length full_name - 2)

(* return 1 if need to skip children without process,
    2 if skip children after process,
    0 otherwise*)
let skip_children (s: stmtkind) = match s with
    |Block _ -> 1
    |UnspecifiedSequence _ -> 2
    |If _ -> 2
    |Loop _ -> 2
    |_ -> 0

let is_binop_ptr (b: pc_binop) = match b with
    | PAddPI | PSubPI | PSubPP -> true
    |_ -> false

let vname_to_string (proc_name: string) (vname: string) =
    let name_strings = [vname;"_ptr_";proc_name] in
    String.concat "" name_strings

let arg_to_string (proc_name: string) (vname,bool_ptr: pc_var) =
    if bool_ptr then vname_to_string proc_name vname
    else let name_strings = [vname;"_";proc_name] in
        String.concat "" name_strings

let ptr_to_string (proc_name: string) ((ptr_name,glob): pc_ptr) =
    let suffix = if glob then "glob" else proc_name in
    let name_strings = [ptr_name;"_ptr_";suffix] in
    String.concat "" name_strings

let varinfo_to_pcvar (v: varinfo) =
    (v.vorig_name,
    (match v.vtype with |TPtr _ -> true |_ -> false))
    (* Pretty_utils.to_string Printer.pp_location loc) *)

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
