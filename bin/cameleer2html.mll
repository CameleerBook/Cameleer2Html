{
  open Html_lib

  let config = Cli.parse ()
  
  let cout = open_out config.output_file
  let print s = Printf.fprintf cout s

  let extract_relative_name f =
    let s = String.split_on_char '/' f in
    List.nth s (List.length s - 1)

  let () = 
    let input_file = extract_relative_name config.input_file in
    Html_gen.preamble cout input_file config.title

  let count = ref 0
  let newline () = Html_gen.newline cout count config.line_numbers

  let () = newline ()

  let keywords = [ "and"; "as"; "assert"; "asr"; "class"; "closed"; 
    "constraint";  "do"; "done"; "downto"; "else"; "exception"; "external"; 
    "for"; "function"; "functor"; "if"; "in"; "include"; "inherit"; "land"; 
    "lazy"; "let"; "lor"; "lsl"; "lsr"; "lxor"; "match"; "method"; "mod"; 
    "mutable"; "new"; "of"; "open"; "or"; "parser"; "private"; "rec"; "then"; 
    "to"; "try"; "virtual"; "when"; "while"; "with"; "module"; "sig"; "struct"; 
    "begin"; "end"; "type"; "val"; "int"; "bool"; "float"; "char"; "string"; 
    "bytes"; "array"; "list"; "option"; "None"; "Some"; "fun"; "'a"; "'b";
    "Bool"; "Float"; "Char"; "String"; "Bytes"; "Array"; "List"; "Printf"; 
    "true"; "false"; "Fun"; "function"; "ensures"; "variant"; "invariant";
    "lemma"; "requires"; "axiom"; "predicate"; "forall"; "exists" ]

  let kw_len = List.length keywords

  let is_keyword =
    let ht = Hashtbl.create kw_len in
    List.iter (fun s -> Hashtbl.add ht s ()) keywords;
    fun s -> Hashtbl.mem ht s

  let process_text = function
    | "<" -> Html_gen.write_text cout "&lt;"
    | ">" -> Html_gen.write_text cout "&gt;"
    | "&" -> Html_gen.write_text cout "&amp;"
    | "\n" -> newline ()
    | _ as s -> Html_gen.write_text cout s
}

let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule scan = parse
  | "(*@" { print "<span class=\"contract\">(*@";
             contract lexbuf;
             print "</span>";
             scan lexbuf }
  | "(*"   { print "<span class=\"comment\">(*";
             comment lexbuf;
             print "</span>";
             scan lexbuf }
  | eof    { () }
  | ident as s
    { if is_keyword s then begin
        print "<span class=\"keyword\">%s</span>" s
      end 
      else process_text s; scan lexbuf }
  (* Cannot bind "\"" with 'as s' and it converts into a char *)
  | "\"" { process_text "\""; string lexbuf; scan lexbuf }
  | "'\"'"
  | _ as s { process_text s; scan lexbuf }
and contract = parse
  | "*)" as s   { process_text s }
  | eof    { () }
  | ident as s
  { if is_keyword s then begin
        print "<span class=\"logical\">%s</span>" s
      end 
      else process_text s; contract lexbuf }
  | "\"" { process_text "\""; string lexbuf; contract lexbuf }
  | "'\"'"
  | _ as s { process_text s; contract lexbuf }
and comment = parse
  | "(*" as s   { process_text s; comment lexbuf; comment lexbuf }
  | "*)" as s   { process_text s }
  | eof    { () }
  | "\"" { process_text "\""; string lexbuf; comment lexbuf }
  | "'\"'"
  | _ as s { process_text s; comment lexbuf }
and string = parse
  | "\"" { process_text "\"" }
  | "\\" _
  | _ as s { process_text s; string lexbuf }

{
  let () =
    scan (Lexing.from_channel (open_in config.input_file));
    Html_gen.postamble cout;
    close_out cout;
    Writer.info "Generated: %s\n" config.output_file
}
