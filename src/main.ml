(**
    The entry point of the plugin.
    This file is responsible for running the plugin.
**)

open Pc

(**
    The function do the following steps:
    - Get the AST and generates the PlusCal program using frama-c visitors
    - Dump the PlusCal program in .tla file.
    - Dump the configuration in .cfg file.
    - If debugging is enabled, dump an additional debug information to a .dump
      file.

    @raise [Sys_error] if there is an error during file operations.
**)
let run() =
    if Options.Enabled.get() then    
    try
        (* Empty pc_prog *)
        let prog = ref ({
            pc_prog_name="";
            pc_constants=[];
            pc_glob_var=[];
            pc_nb_process=0;
            pc_processus=[];
            pc_entry_point="";
            pc_procedures=[]}
        ) in
        (* Generate pc_prog *)
        Visitor.visitFramacFileSameGlobals (new Pc_gen.gen_pc prog) (Ast.get ());

        (* Open formatter for .tla and .cfg files *)
        let prog_name = (!prog).pc_prog_name in 
        let tla_chan = open_out (prog_name^".tla") in
        let cfg_chan = open_out (prog_name^".cfg") in
        let tla_fmt = Format.formatter_of_out_channel tla_chan in
        let cfg_fmt = Format.formatter_of_out_channel cfg_chan in

        (* Dump pc_prog to .tla and .cfg files *)
        Pc_dump.dump_prog tla_fmt !prog;
        Options.Self.result "%s.tla file is generated." prog_name;
        Config_dump.dump_config cfg_fmt !prog;
        Options.Self.result "%s.cfg file is generated." prog_name;
        close_out tla_chan;
        close_out cfg_chan;

        (* If `-debug-dump` option is enabled, output a .dump file*)
        if Options.DebugDump.get() then
            let dump_file_name = prog_name^".dump" in
            let out_chan = open_out dump_file_name in
            let out_fmt = Format.formatter_of_out_channel out_chan in

            Pc_debug.print_prog out_fmt !prog;
            Options.Self.result "%s.dump file is generated." 
                (prog_name);
            close_out out_chan;

    with Sys_error _ as exc ->
        let msg = Printexc.to_string exc in
        Printf.eprintf "Error in the run() function: %s\n" msg

let () = Boot.Main.extend run