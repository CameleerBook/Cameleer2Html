val error : string -> 'a
val info : ('a -> 'b, out_channel, unit) format -> 'a -> 'b
val format : ('a -> 'b, unit, string) format -> 'a -> 'b
val write : out_channel -> string -> unit