(**
    Utilities for parsing configuration invariants.
**)

open Yojson.Safe


(** Represents a local entry with variable name, process name, process ID, and expected value. **)
type local_entry = {
  var_name: string;      (** The name of the variable. *)
  expected_val: string;  (** The expected value of the variable. *)
  proc_name: string;     (** The name of the process. *)
  proc_id: int;          (** The ID of the process. *)
}

(** Represents a global entry with variable name and expected value. **)
type global_entry = {
  var_name: string;      (** The name of the variable. *)
  expected_val: string;  (** The expected value of the variable. *)
}

(**
  Type representing a configuration entry of a .expect file
  Can be either local or global, in a tuple with the process name and id, in which the check is performed
**)
type entry =
  | Local of local_entry * (string * int)  (* A local entry *)
  | Global of global_entry * (string * int)  (* A global entry *)



(**
  Parses a JSON object to extract configuration details
  and returns a result containing either a [Global] or [Local] entry, or an error message.
  @param json JSON object to parse.
  @return [Result] containing entry parsed of JSON file or an error message.

  The JSON object is expected to have the following fields:
    - ["var_name"]: String representing the variable name.
    - ["expected_val"]: String representing the expected value.
    - ["proc_to_check"]: Tuple containing a string (process name) and an integer (process ID).
    - ["proc_of_var"]: (Optional) Tuple containing a string (process name) and an integer (process ID) for local entries.
**)
let entry_of_json json =
  let get_string_field field =
    match Util.member field json with
    | `String s -> Result.ok s
    | _ -> Result.error (Printf.sprintf "Error: entry_of_json: missing or invalid field '%s'\n" field)
  in
  let get_proc_field field =
    match Util.member field json with
    | `Tuple (`String s::[`Int i]) -> Result.ok (s,i)
    | _ -> Result.error (Printf.sprintf "Error: entry_of_json: missing or invalid field '%s'\n" field)
  in

  let var_name_result = get_string_field "var_name" and
      expected_val_result = get_string_field "expected_val" and
      proc_to_check_result = get_proc_field "proc_to_check" and
      proc_of_var_exists = Util.member "proc_of_var" json
  in

  match var_name_result, expected_val_result, proc_to_check_result with
  | Ok var_name, Ok expected_val, Ok proc_to_check ->
      if proc_of_var_exists = `Null then
        Result.ok (Global ({var_name = var_name; expected_val = expected_val}, proc_to_check))

      else
        (match get_proc_field "proc_of_var" with
        | Ok (proc_name, proc_id) ->
            Result.ok (Local ({var_name = var_name; expected_val = expected_val;
                               proc_name = proc_name; proc_id = proc_id}, proc_to_check))
        | Error e -> Result.error e)

  | Error e,_,_ | _,Error e,_ | _,_,Error e -> Result.error e


(**
  Parses a JSON file containing configuration entries.
  The JSON file is expected to contain a list of entries.
  @param filename path to the JSON file to be parsed.
  @return [entry List] parsed from the JSON file.

  @raise [Json_error] if the JSON file does not contain a list of entries or if there is an error parsing the file.
**)
let parse_expect_file filename =
  try
    let json = Yojson.Safe.from_file filename in
    match json with
      |`List entries -> List.map entry_of_json entries
      |_ -> failwith "Expect file must be a list of entries"
  with
    |Yojson.Json_error msg -> failwith ("Error parsing expect file: " ^ msg)


