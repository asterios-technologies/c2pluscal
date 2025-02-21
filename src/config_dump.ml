open Pc
open Config_utils

let dump_config out (prog: pc_prog) =
  Format.fprintf out "CONSTANTS\n";
  (List.iteri (
    fun i proc -> Format.fprintf out "  %s = {%s}\n"
    proc.pc_process_set
    (string_of_int i))
  prog.pc_processus);
  Format.fprintf out "  UNDEF = UNDEF\n";
  Format.fprintf out "  defaultInitValue = 0\n";
  Format.fprintf out "\n";
  Format.fprintf out "CONSTANTS\n";
  (List.iter (
    fun (cst_name,cst_val) ->
    Format.fprintf out "  %s = %i\n" cst_name cst_val)
  prog.pc_constants);
  Format.fprintf out "\n";
  Format.fprintf out "SPECIFICATION Spec\n";
  Format.fprintf out "\n";
  Format.fprintf out "PROPERTY\n";
  Format.fprintf out "\n";
  Format.fprintf out "\n";
  Format.fprintf out "INVARIANT\n";
  let expect_file = Options.ExpectVal.get() in
  if String.length expect_file > 0 then
    let expect_entries = Config_utils.parse_expect_file expect_file in
    List.iter (
      fun entry ->
      Format.fprintf out "  (pc[%i] = \"Check_%s\" => load(my_stack[%i], %s_ptr_%s[%i]) = %s)\n" entry.proc_id entry.proc_name entry.proc_id entry.var_name entry.proc_name entry.proc_id entry.expect_val;)
    expect_entries;
  Format.fprintf out "\n";