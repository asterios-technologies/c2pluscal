(**
    Dump the configuration of a PlusCal program.
**)

open Pc

(**
  Outputs the configuration of a PlusCal program [prog] to the formatter [out].
  @param out formatter to output the configuration to.
  @param prog PlusCal program whose configuration is being dumped.
  @return unit.

  The configuration includes:
  - The constants section, where each process set is listed with an index.
  - The constants section again, listing each constant name and its value.
  - The specification section.
  - The property section.
  - The invariant section, which includes expected values from a file if specified in the options.
*)
let dump_config out (prog: pc_prog) =
  Format.fprintf out "CONSTANTS\n";
  (*Dumps sets in which process are taken*)
  (List.iteri (
    fun i proc -> Format.fprintf out "  %s = {%s}\n"
    proc.pc_process_set
    (string_of_int i))
  prog.pc_processus);
  Format.fprintf out "  UNDEF = UNDEF\n";
  Format.fprintf out "  defaultInitValue = 0\n";
  Format.fprintf out "\n";

  Format.fprintf out "CONSTANTS\n";
  (*Dumps TLA constant of the program*)
  (List.iter (
    fun (cst_name, cst_val) ->
    Format.fprintf out "  %s = %i\n" cst_name cst_val)
  prog.pc_constants);
  Format.fprintf out "\n";

  Format.fprintf out "SPECIFICATION Spec\n";
  Format.fprintf out "\n";
  Format.fprintf out "PROPERTY\n";
  Format.fprintf out "\n";
  Format.fprintf out "\n";

  (*Dumps Invariant, if the options is given to Frama-C*)
  Format.fprintf out "INVARIANT\n";
  let expect_file = Options.ExpectVal.get() in
  if String.length expect_file > 0 then
    Format.fprintf out "  Inv\n";
  Format.fprintf out "\n";