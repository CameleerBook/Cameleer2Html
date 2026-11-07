open Writer
open Metadata

let preamble c (t: string) =
  let elems = [
    "<!DOCTYPE html>";
    "<head>";
    "<link rel=\"stylesheet\" href=\"index.css\">";
    format "<title>%s</title>" t;
    "</head>";
    "<body>";
    "<div class=\"title\">";
    format "<h2>%s</h2>" t;
    "</div>";
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