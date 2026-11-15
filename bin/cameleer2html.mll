{
  open Html_lib

  let config = Cli.parse ()
  
  let cout = open_out config.output_file
  let print s = Printf.fprintf cout s

  let () = Html_gen.preamble cout config.title

  let count = ref 0
  let newline () = 
    if config.line_numbers then begin
      incr count; print "\n<span class=\"number\">%d</span> " !count
    end
    else print "\n"
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
    | "<" -> print "&lt;"
    | ">" -> print "&gt;"
    | "&" -> print "&amp;"
    | "\n" -> newline ()
    | _ as s -> print "%s" s
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
      else print "%s" s; scan lexbuf }
  | '"'    { print "\""; string lexbuf; scan lexbuf }
  | "'\"'"
  | _ as s { process_text s; scan lexbuf }
and contract = parse
  | "*)"   { print "*)" }
  | eof    { () }
  | ident as s
  { if is_keyword s then begin
        print "<span class=\"logical\">%s</span>" s
      end 
      else print "%s" s; contract lexbuf }
  | '"'    { print "\""; string lexbuf; contract lexbuf }
  | "'\"'"
  | _ as s { process_text s; contract lexbuf }
and comment = parse
  | "(*"   { print "(*"; comment lexbuf; comment lexbuf }
  | "*)"   { print "*)" }
  | eof    { () }
  | '"'    { print "\""; string lexbuf; comment lexbuf }
  | "'\"'"
  | _ as s { process_text s; comment lexbuf }
and string = parse
  | '"'    { print "\"" }
  | "\\" _
  | _ as s { process_text s; string lexbuf }

{
  let () =
    scan (Lexing.from_channel (open_in config.input_file));
    Html_gen.postamble cout;
    close_out cout;
    Writer.info "Generated: %s\n" config.output_file
}
