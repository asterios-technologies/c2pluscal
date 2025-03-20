(**
    Utilities for generating PlusCal program.
**)

open Cil_types
open Pc


(*****************)
(*   TO STRING   *)
(*****************)

(**
  Converts an expression to its string representation.
  @param exp The expression to convert.
  @return A string representing the expression.
**)
let exp_to_str = function
  | SizeOfE _ -> "SizeOfE"
  | SizeOfStr _ -> "SizeOfStr"
  | AlignOf _ -> "AlignOf"
  | AlignOfE _ -> "AlignOfE"
  | CastE _ -> "CastE"
  | StartOf _ -> "StartOf"
  |_ -> ""

(**
  Converts an instruction to its string representation.
  @param instr The instruction to convert.
  @return A string representing the instruction.
**)
let instr_to_str = function
  | Asm _ -> "Asm"
  | Skip _ -> "Skip"
  | Code_annot _ -> "Code Annot"
  |_ -> ""

(**
  Converts a statement to its string representation.
  @param stmt The statement to convert.
  @return A string representing the statement.
**)
let stmt_to_str = function
  | TryFinally _ | TryExcept _ | TryCatch _ -> "Try"
  | Throw _ -> "Throw"
  | Break _ -> "Break"
  | Continue _ -> "Continue"
  | Switch _ -> "Switch"
  | Loop _ -> "Loop"
  | UnspecifiedSequence _ -> "UnspecifiedSequence"
  | _ -> ""



(****************************)
(*   File Level Functions   *)
(****************************)

(**
    Retrieves the base name of the first file in the list of files managed by the Kernel.
    @return A string representing the base name of the first file.
**)
let get_file_name() =
    let full_name =  Filepath.Normalized.to_pretty_string (List.hd (Kernel.Files.get())) in
    let base_name = Filename.basename full_name in
    (*Remove ".c" of the file name*)
    String.sub base_name 0 (String.length base_name - 2)

(**
    Returns the name of the entry point function in the given file [f].
    The entry point is determined by finding the last function definition in the file's
    global declarations.

    @param f The file to search for the entry point.
    @return The name of the entry point function.
**)
let get_entry_point (f: file) =
    List.hd (List.rev
        (List.filter_map (fun g ->
            match g with
                | GFun(fundec,_) -> Some fundec.svar.vorig_name
                | _ -> None)
        f.globals))



(*********************)
(*   TRANSFORMS PC   *)
(*********************)

(**
    Checks if a given binary operation is a pointer operation.
    @param b binary operation to check.
    @return [Boolean] telling if the operation is on pointer or not.
**)
let is_binop_ptr (b: pc_binop) = match b with
    | PAddPI | PSubPI | PSubPP -> true
    |_ -> false

(**
    Adds the integer [n] to a pc_expr [e].

    @param e [pc_expr] to which the integer [n] will be added.
    @param n integer to add to the constant within [e].
    @return [pc_expr] with the integer [n] added to the expression [e].
**)
let add_pc_cst (e: pc_expr) (n: int) = PBinop (PAdd, e, PCst(PInt(n)))


(******************)
(*   DUMP UTILS   *)
(******************)

(**
    Computes the variable name in the PlusCal representation.
    @param proc_name name of the procedure.
    @param vname name of the variable.
    @return [String] that combines the variable name, "_ptr_", and the procedure name.

    Useful to dump procedure variables section.
**)
let vname_to_string (proc_name: string) (vname: string) =
    let name_strings = [vname;"_ptr_";proc_name] in
    String.concat "" name_strings

(**
    Computes the argument name in the PlusCal representation.
    @param proc_name name of the procedure.
    @param vname name of the argument.
    @return [String] that combines [vname], an underscore, and [proc_name].

    Useful to dump procedure arguments.
**)
let arg_to_string (proc_name: string) (vname: pc_var) =
    let name_strings = [vname;"_";proc_name] in
    String.concat "" name_strings

(**
    Converts a pointer PlusCal representation to a string.
    @param proc_name name of the procedure.
    @param (ptr_name, glob) tuple where [ptr_name] is the name of the pointer
                              and [glob] is a boolean indicating if the pointer is global.
    @return [String] representation of the pointer in the format "ptr_name_ptr_suffix",
                       where suffix is either "glob" if the pointer is global, or the procedure name if it is not.

    Useful to dump pointer in the program core.
**)
let ptr_to_string (proc_name: string) ((ptr_name,glob): pc_ptr) =
    let suffix = if glob then "glob" else proc_name in
    let name_strings = [ptr_name;"_ptr_";suffix] in
    String.concat "" name_strings

(**
    Recursively prints the elements of a list [l] to the formatter [out] using the function [dump_fun].
    @param out formatter to output the list elements.
    @param l list of elements to be printed.
    @param dump_fun function used to print each element of the list.
    @return unit.

    Each element is separated by a comma, except for the last element.
**)
let rec dump_list out l dump_fun =
    match l with
        | [] -> Format.fprintf out ""
        | t::[] -> dump_fun out t
        | t::q -> dump_fun out t; Format.fprintf out ","; dump_list out q dump_fun

(**
    Computes a string consisting of [n * 4] spaces.
    @param n number of indentation levels.
    @return [String] with [n * 4] spaces.

    Useful for creating indentation in formatted output.
**)
let string_of_indent n = (String.make (n * 4) ' ')

(**
    Computes a new string with [n * 4] spaces concatened to the given [indent] string.
    @param n number of indentation levels.
    @param indent The string to which the indentation will be added.
    @return [String] with the wanted indentation.
**)
let add_indent n indent = String.concat "" [(string_of_indent n); indent]

(**
    Removes the last character from the string [s].
    @param s input string from which the last character will be removed.
    @return [String] with the last character removed, or the original string if it is empty.

    Useful to remove ":" from labels.
**)
let remove_last_char s =
    if String.length s = 0 then s
    else String.sub s 0 (String.length s - 1)



(***********************)
(*   PROCEDURE UTILS   *)
(***********************)

(**
    Recursively processes a list of variables and generates a list of declarations and initializations.
    @param acc tuple containing the current list of declarations and the number of declarations.
    @param vars list of tuples where each tuple contains a variable name and an optional array size.
    @param args_info tuple containing a boolean indicating if the variables are arguments,
                       a list of indices for array arguments, and the number of arguments.
    @return [Pc_instr list, Integer] tuple containing the updated list of declarations and the number of declarations.

    The number of declarations is useful to return to generate the right number of pop instructions later.

    We can not use [is_args] variable in the case of an array, because Frama-C transforms them as pointer
    when called in a function, then the variable will not be considerd as an array and will have [None] as array size.
**)
let rec procedure_push_vars acc (vars: (pc_var * int option) list) (args_info: (bool * int list * int))  =
    let (is_args, array_args_idx, nb_args) = args_info and
        (decl_list, nb_decl) = acc in
    match vars with
        | [] -> acc

        (*Declaration of an array, add PInitArray instruction*)
        | (vname, Some array_size)::q ->
            procedure_push_vars
            ((PDecl (PUndef, (vname, false))::(PInitArray (array_size, (vname, false))::decl_list)), nb_decl+1)
            q args_info

        | (vname, None)::q ->
            if is_args
            then
                let idx = nb_args - ((List.length q)+1) in
                if List.mem idx array_args_idx
                (*If the current index of the argument is the index of an array*)
                (*Add a copy instruction, to pass the array as a pointer*)
                then procedure_push_vars
                     ((PDecl (PUndef, (vname, false))::(PCopy (PArg (vname), (vname, false))::decl_list)), nb_decl+1)
                     q args_info

                (*Simple argument declaration with its value*)
                else procedure_push_vars
                     ((PDecl (PArg((vname)), (vname, false))::decl_list), nb_decl+1)
                     q args_info

            (*Not an argument, just declare with undef value*)
            else procedure_push_vars ((PDecl (PUndef, (vname, false))::decl_list), nb_decl+1) q args_info

(**
    Recursively generates a list of pop operations.
    @param acc current list of operations.
    @param n number of pop operations to generate.
    @return[Pc_instr list] of operations with the specified number of pop operations added.
**)
let rec procedure_pop acc n =
    match n with
        | 0 -> acc
        | _ -> procedure_pop (PPop::acc) (n-1)



(**************************)
(*   ARRAY AS ARGUMENTS   *)
(**************************)

(**
    Converts a variable information into a representation, with information if it is an array or not.
    @param v variable information record of type [varinfo].
    @return [String, Int option] tuple containing the original name of the variable and an optional
            length of the array if the variable is of array type.

    Useful to generate init_array operations.
**)
let varinfo_is_array (v: varinfo) =
    match v.vtype with
        | TArray(_,e,_) -> (v.vorig_name, (Some (Cil.lenOfArray e)))
        | _ -> (v.vorig_name, None)

(**
    Detects all the StartOf operations (i.e., array conversion to pointer) in function calls within a list of statements
    and adds the array argument indices to a hashtable with the function name as the key.

    @param array_args_table hashtable with the function name as the key and a list of argument indices as the value.
    @param stmts list of statements to analyze.
    @return unit.

    The hashtable will be modified in place and do not need to be returned.
**)
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

(**
    Iterates over all global definitions to find functions and detect array arguments in their statements.
    @param array_args_table hashtable with the function name as the key and a list of argument indices as the value.
    @param globals list of global definitions to analyze.
    @return unit.

    The hashtable will be modified in place and do not need to be returned.
**)
let get_all_array_args (array_args_table: (string, int) Hashtbl.t) (globals: global list) =
    List.iter (fun g ->
        match g with
            |GFun(fundec,_) -> (detect_array_args array_args_table fundec.sbody.bstmts)
            |_ -> ()) globals



(**************)
(*   OTHERS   *)
(**************)

(**
    Folds a function [f] over a list [l] with an initial accumulator [init_acc].
    If any application of [f] returns an error, the folding process stops and
    returns the error. Otherwise, it combines the results using the function [g].

    @param f function that takes an element of the list and returns a result.
    @param g function that combines the accumulator and the result of [f].
    @param init_acc initial value of the accumulator.
    @param l list to fold over.
    @return [Result] containing the final accumulated value or an error.
**)
let fold_left_result f g init_acc l =
    List.fold_left (fun acc i ->
        Result.bind acc (fun ok_acc ->
            Result.bind (f i) (fun ok_i ->
                Result.ok (g ok_acc ok_i))))
    (Result.ok init_acc) l