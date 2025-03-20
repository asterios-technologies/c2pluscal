(**
    Main entry point of the plugin.
    This file is responsible for running the plugin.
**)

open Pc

(**
    Runs the PlusCal plugin if the corresponding option is enabled.
    - Parses the AST and generates the PlusCal program.
    - Dumps the PlusCal program to a .tla file.
    - Dumps the configuration to a .cfg file.
    - If debugging is enabled, dumps additional debug information to a .dump file.

    @raise [Sys_error] if there is an error during file operations.
**)
let run() =
    if Options.Enabled.get() then
    try
        (*Empty pc_prog*)
        let prog = ref ({pc_prog_name="";
                        pc_constants=[];
                        pc_glob_var=[];
                        pc_nb_process=0;
                        pc_processus=[];
                        pc_entry_point="";
                        pc_procedures=[]})
        in
        (*Generates pc_prog*)
        Visitor.visitFramacFileSameGlobals (new Pc_gen.gen_pc prog) (Ast.get ());

        (*Opens formatter for TLA spec and cfg file*)
        let tla_chan = open_out (String.concat "" [(!prog).pc_prog_name;".tla"]) in
        let cfg_chan = open_out (String.concat "" [(!prog).pc_prog_name;".cfg"]) in
        let tla_fmt = Format.formatter_of_out_channel tla_chan in
        let cfg_fmt = Format.formatter_of_out_channel cfg_chan in

        (*Dumps pc_prog to tla and cfg file*)
        Pc_dump.dump_prog tla_fmt !prog;
        Config_dump.dump_config cfg_fmt !prog;
        close_out tla_chan;
        close_out cfg_chan;

        (*If debug dump option enabled, outputs it in .dump file*)
        if Options.DebugDump.get() then
            let out_chan = open_out (String.concat "" [(!prog).pc_prog_name;".dump"]) in
            let out_fmt = Format.formatter_of_out_channel out_chan in

            Pc_debug.print_prog out_fmt !prog;
            close_out out_chan;

    with Sys_error _ as exc ->
        let msg = Printexc.to_string exc in
        Printf.eprintf "Error in the run() function: %s\n" msg

let () = Boot.Main.extend run