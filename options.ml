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

module BlockMain = Self.False
  (struct
    let option_name = "-block-main"
    let help = "when on (off by default), blocks the main function of the translated PlusCal file,\n
                just before it returns, to write properties and invariants on variables of the program."
  end)