val preamble : out_channel -> string -> string -> unit
val postamble: out_channel -> unit
val newline: out_channel -> int ref -> bool -> unit
val write_text: out_channel -> string -> unit