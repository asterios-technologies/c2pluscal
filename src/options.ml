(** Options of the plugin. **)

module Self = Plugin.Register
  (struct
    let name = "C2PlusCal Transpiler"
    let shortname = "C2PlusCal"
    let help = ("C2PlusCal translates C source code to a PlusCal model\n"^
                "that can be checked by TLA+ verification techniques.")
  end)

module Enabled = Self.False
  (struct
    let option_name = "-pluscal"
    let help = "when on (off by default), translates the C file to PlusCal specification."
  end)

module DebugDump = Self.False
  (struct
    let option_name = "-debug-dump"
    let help = "When on (off by default), dumps a .dump file useful for translation debug."
  end)

module CheckFun = Self.String_list
  (struct
    let option_name = "-check-label"
    let help = (
      "Add a label to the functions whose name is given in the translated\n
       PlusCal file, just before they return, to write properties and\n
       invariants on variables of the program.")
    let arg_name = "fun_list"
  end)

module ExpectVal = Self.String
  (struct
    let option_name = "-expect"
    let default = ""
    let arg_name = "expect-file"
    let help = "given file with the expecting value of variables of the program."
  end)
