(**
  Options of the plugin.
  The options are:
    - pluscal: when on (off by default), translates the C file to PlusCal specification.
    - pluscal-output: file where the pluscal spec is output.
    - debug-dump: when on (off by default), dumps a .dump file useful for translation debug.
    - check-label: add a label to the functions whose name is given, in the translated PlusCal file,
                  just before they return, to write properties and invariants on variables of the program.
    - expect: given file with the expecting value of variables of the program.
**)

module Self = Plugin.Register
  (struct
    let name = "pluscal transpiler"
    let shortname = "pluscal"
    let help = "pluscal transpilation"
  end)

module Enabled = Self.False
  (struct
    let option_name = "-pluscal"
    let help = "when on (off by default), translates the C file to PlusCal specification."
  end)

module DebugDump = Self.False
  (struct
    let option_name = "-debug-dump"
    let help = "when on (off by default), dumps a .dump file useful for translation debug."
  end)

module CheckFun = Self.String_list
  (struct
    let option_name = "-check-label"
    let help = "add a label to the functions whose name is given, in the translated PlusCal file,\n
                just before they return, to write properties and invariants on variables of the program."
    let arg_name = "fun_list"
  end)

module ExpectVal = Self.String
  (struct
    let option_name = "-expect"
    let default = ""
    let arg_name = "expect-file"
    let help = "given file with the expecting value of variables of the program."
  end)