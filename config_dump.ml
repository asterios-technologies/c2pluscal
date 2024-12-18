open Pc

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
  Format.fprintf out "SPECIFICATION Spec\n";
  Format.fprintf out "\n";
  Format.fprintf out "PROPERTY\n";
  Format.fprintf out "  Prop\n";
  Format.fprintf out "\n";
  Format.fprintf out "INVARIANT\n";
  Format.fprintf out "  Inv\n"
