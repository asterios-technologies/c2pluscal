let help_msg = "control flow graph computation and display"

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