type internal_config = {
  mutable input_file : string option;
  mutable title : string option;
  mutable line_numbers : bool;
  mutable output_file : string option;
}

type config = {
  input_file : string;
  title : string;
  line_numbers : bool;
  output_file : string;
}

let iconfig : internal_config = {
  input_file = None;
  title = None;
  line_numbers = false;
  output_file = None;
}

let print_version () =
  Writer.info "Cameleer2Html %s\n" Version.version;
  exit 0

let set_input_file arg =
  if not (Sys.file_exists arg) then
    raise (Arg.Bad "File does not exist")
  else if not (Filename.check_suffix arg ".ml") then
    raise (Arg.Bad "Not an OCaml file")
  else
    iconfig.input_file <- Some arg

let set_output_file s =
  let s' = if String.ends_with ~suffix:".html" s then s else s ^ (".html") in
  iconfig.output_file <- Some s'

let set_title s =
  iconfig.title <- Some s

let spec = [
  ("--version", Arg.Unit print_version, "Print version information");
  ("-l", Arg.Unit (fun () -> iconfig.line_numbers <- true), "Show line numbers");
  (*("-o", Arg.String set_output_file, "Set html output file");
  ("--output", Arg.String set_output_file, "Set html output file");*)
  ("-t", Arg.String set_title, "Set html header title")
]

let usage_msg =
  "cameleer2html [options] <file.ml> <title>\n"

let usage () =
  Arg.usage spec usage_msg;
  exit 1

let anon_fun arg =
  match iconfig.input_file with
  | None -> set_input_file arg
  | Some _ -> raise (Arg.Bad "Too many positional arguments")

let make_external_config f =
  if iconfig.title = None then set_title f;
  if iconfig.output_file = None then set_output_file f;

  {
    input_file = f;
    title = Option.get iconfig.title;
    line_numbers = iconfig.line_numbers;
    output_file = Option.get iconfig.output_file;
  }

let parse () =
  Arg.parse spec anon_fun usage_msg;

  match iconfig.input_file with
  | Some input_file -> make_external_config input_file
  | _ -> usage ()