{
  let print_error s =
    Printf.eprintf "error: %s\n" s;
    exit 1

  let file = Sys.argv.(1)

  let title = Sys.argv.(2)

  let print_line_numbers = ref true

  let () =
    if not (Sys.file_exists file) then begin
      print_error ("File not found: " ^ file)
    end;
    let num_args = Array.length Sys.argv in
    if num_args = 4 then begin
      print_line_numbers := bool_of_string Sys.argv.(3)
    end else if num_args <> 3 then
      print_error "Wrong number of arguments"

  
  let cout = open_out (file ^ ".html")
  let print s = Printf.fprintf cout s

  let () =
    print "<!DOCTYPE html>";
    print "<head><link rel=\"stylesheet\" href=\"index.css\"><title>%s</title></head>" title;
    print "<body>";
    print "<div class=\"title\"><h2>%s</h2></div>" title;
    print "<pre>"

  let count = ref 0
  let newline () = 
    if !print_line_numbers then begin
      incr count; print "\n<span class=\"number\">%3d</span> " !count
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
  let url = "https://github.com/PedroGasparinho/Cameleer2Html"
  let url_name = "Cameleer2Html"
  let info_text = "This page was generated with "

  let () =
    scan (Lexing.from_channel (open_in file));
    print "</pre>\n";
    print "<div class=\"info\">\n";
    print "<p>%s<a href=\"%s\">%s</a><p>\n" info_text url url_name;
    print "</div>\n";
    print "</body></html>\n";
    close_out cout

}
