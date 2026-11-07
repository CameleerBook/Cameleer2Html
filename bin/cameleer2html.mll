{
  open Html_lib

  let file, title, print_line_numbers =
    match Array.length Sys.argv with
    | 3 -> Sys.argv.(1), Sys.argv.(2), true
    | 4 ->
        Sys.argv.(1),
        Sys.argv.(2),
        bool_of_string Sys.argv.(3)
    | _ ->
        Writer.error "Usage: main <file> <title> [line_numbers]"
  
  let print_line_numbers = ref print_line_numbers
  
  let cout = open_out (file ^ ".html")
  let print s = Printf.fprintf cout s

  let () = Html_gen.preamble cout title

  let count = ref 0
  let newline () = 
    if !print_line_numbers then begin
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
  | "<"    { print "&lt;"; scan lexbuf }
  | ">"    { print "&gt;"; scan lexbuf }
  | "&"    { print "&amp;"; scan lexbuf }
  | "\n"   { newline (); scan lexbuf }
  | '"'    { print "\""; string lexbuf; scan lexbuf }
  | "'\"'"
  | _ as s { print "%s" s; scan lexbuf }
and contract = parse
  | "*)"   { print "*)" }
  | eof    { () }
  | ident as s
  { if is_keyword s then begin
        print "<span class=\"logical\">%s</span>" s
      end 
      else print "%s" s; contract lexbuf }
  | "\n"   { newline (); contract lexbuf }
  | '"'    { print "\""; string lexbuf; contract lexbuf }
  | "<"    { print "&lt;"; contract lexbuf }
  | ">"    { print "&gt;"; contract lexbuf }
  | "&"    { print "&amp;"; contract lexbuf }
  | "'\"'"
  | _ as s { print "%s" s; contract lexbuf }
and comment = parse
  | "(*"   { print "(*"; comment lexbuf; comment lexbuf }
  | "*)"   { print "*)" }
  | eof    { () }
  | "\n"   { newline (); comment lexbuf }
  | '"'    { print "\""; string lexbuf; comment lexbuf }
  | "<"    { print "&lt;"; comment lexbuf }
  | ">"    { print "&gt;"; comment lexbuf }
  | "&"    { print "&amp;"; comment lexbuf }
  | "'\"'"
  | _ as s { print "%s" s; comment lexbuf }
and string = parse
  | '"'    { print "\"" }
  | "<"    { print "&lt;"; string lexbuf }
  | ">"    { print "&gt;"; string lexbuf }
  | "&"    { print "&amp;"; string lexbuf }
  | "\\" _
  | _ as s { print "%s" s; string lexbuf }

{
  let () =
    scan (Lexing.from_channel (open_in file));
    Html_gen.postamble cout;
    close_out cout;
    Writer.info "Generated: %s\n" (file ^ ".html")
}
