let help_msg = "pluscal transpilation"

module Self = Plugin.Register
  (struct
    let name = "pluscal transpiler"
    let shortname = "pluscal"
    let help = help_msg
  end)

module Enabled = Self.False
  (struct
    let option_name = "-pluscal"
    let help = "when on (off by default), translates the C file to PlusCal specification."
  end)

module OutputFile = Self.String
  (struct
    let option_name = "-pluscal-output"
    let default = "c.tla"
    let arg_name = "output-file"
    let help = "file where the pluscal spec is output."
  end)

module DebugDump = Self.False
  (struct
    let option_name = "-debug-dump"
    let help = "when on (off by default), dumps a .out file useful for translation debug."
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