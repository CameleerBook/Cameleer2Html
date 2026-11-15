type config = {
  input_file : string;
  title : string;
  line_numbers : bool;
  output_file : string;
}

val parse : unit -> config