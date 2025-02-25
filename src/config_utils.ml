(**
    Utilities for generating configuration invariants.
**)

open Yojson.Safe.Util

(**
  Type representing a configuration entry of a .expect file

  @param var_name name of the variable.
  @param proc_name name of the process.
  @param proc_id id of the process.
  @param expect_val expected value for the variable.
**)
type entry = {
  var_name: string;
  proc_name: string;
  proc_id: int;
  expect_val: string;
}

(**
  Converts a JSON object into a configuration entry
  - @param json JSON object to be converted.
  - @return [entry] extracted from the JSON object.
**)
let entry_of_json json =
  {
    var_name = json |> member "var_name" |> to_string;
    proc_name = json |> member "proc_name" |> to_string;
    proc_id = json |> member "proc_id" |> to_int;
    expect_val = json |> member "expect_val" |> to_string;
  }

(**
  Parses a JSON file containing configuration entries.
  The JSON file is expected to contain a list of entries.
  - @param filename path to the JSON file to be parsed.
  - @return [entry List] parsed from the JSON file.

  - @raise Failure if the JSON file does not contain a list of entries or if there is an error parsing the file.
**)
let parse_expect_file filename =
  try
    let json = Yojson.Safe.from_file filename in
    match json with
      |`List entries -> List.map entry_of_json entries
      |_ -> failwith "Expect file must be a list of entries"
  with
    |Yojson.Json_error msg -> failwith ("Error parsing expect file: " ^ msg)


