open Writer
open Metadata

let preamble c (f: string) (t: string) =
  let elems = [
    "<!DOCTYPE html>\n";
    "<head>\n";
    "<link rel=\"stylesheet\" href=\"index.css\">\n";
    "<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,400,0,0&icon_names=download\"/>\n";
    format "<title>%s</title>\n" t;
    "</head>\n";
    "<body>\n";
    "<div class=\"title\">\n";
    format "<h2>%s</h2>\n" t;
    format "<a href=\"%s\" download=\"%s\">\n" f f;
    "<span class=\"material-symbols-outlined\">";
    "download";
    "</span>\n";
    "</a>\n";
    "</div>\n";
    "<pre>"
  ] in
  List.iter (fun x -> write c x) elems

let postamble c =
  let md = Metadata.get() in
  let elems = [
    "\n</pre>\n";
    "<div class=\"info\">\n";
    "<p>This page was generated with ";
    format "<a href=\"%s\">%s</a><p>\n" md.url md.name;
    "</div>\n";
    "<div class=\"metadata\">\n";
    format "<p>Version: %s; Datetime: %s</p>\n" md.version md.date;
    "</div>\n";
    "</body></html>"
  ] in
  List.iter (fun x -> write c x) elems

let newline c count line_numbers = 
  if line_numbers then begin
    incr count;
    let elems = [
      format "\n<span id=\"%d\" class=\"number\">" !count;
      format "<a href=\"#%d\">%d</a>" !count !count;
      "</span>"
    ] in
    List.iter (fun x -> write c x) elems
  end
  else write c "\n"

let write_text c s = 
  write c s