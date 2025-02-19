open Pc

let run() =
    try
        let prog = ref ({pc_prog_name="";
                        pc_constants=[];
                        pc_glob_var=[];
                        pc_nb_process=0;
                        pc_processus=[];
                        pc_entry_point="";
                        pc_procedures=[]}) in
        Visitor.visitFramacFileSameGlobals (new Pc_gen.gen_pc prog) (Ast.get ());
        let tla_chan = open_out (String.concat "" [(!prog).pc_prog_name;".tla"]) in
        let cfg_chan = open_out (String.concat "" [(!prog).pc_prog_name;".cfg"]) in
        let tla_fmt = Format.formatter_of_out_channel tla_chan in
        let cfg_fmt = Format.formatter_of_out_channel cfg_chan in
        Pc_dump.dump_prog tla_fmt !prog;
        Config_dump.dump_config cfg_fmt !prog;
        close_out tla_chan;
        close_out cfg_chan;
        if Options.DebugDump.get() then
            let out_chan = open_out (String.concat "" [(!prog).pc_prog_name;".dump"]) in
            let out_fmt = Format.formatter_of_out_channel out_chan in
            Pc_print.print_prog out_fmt !prog;
            close_out out_chan;
    with Sys_error _ as exc ->
        let msg = Printexc.to_string exc in
        Printf.eprintf "There was an error: %s\n" msg

let () = Boot.Main.extend run