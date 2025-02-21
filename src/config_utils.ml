open Yojson.Safe.Util

type entry = {
  var_name: string;
  proc_name: string;
  proc_id: int;
  expect_val: string;
}

let entry_of_json json =
  {
    var_name = json |> member "var_name" |> to_string;
    proc_name = json |> member "proc_name" |> to_string;
    proc_id = json |> member "proc_id" |> to_int;
    expect_val = json |> member "expect_val" |> to_string;
  }

let parse_expect_file filename =
  try
    let json = Yojson.Safe.from_file filename in
    match json with
      |`List entries -> List.map entry_of_json entries
      |_ -> failwith "Expect file must be a list of entries"
  with
    |Yojson.Json_error msg -> failwith ("Error parsing expect file: " ^ msg)


