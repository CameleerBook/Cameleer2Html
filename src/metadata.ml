open Unix

type t = {
  version: string;
  date: string;
  url: string;
  name: string;
}

let date () = 
  let date = Unix.localtime (Unix.time ()) in
  let day = date.tm_mday
  and mon = date.tm_mon + 1
  and year = date.tm_year + 1900
  and hour = date.tm_hour
  and min = date.tm_min
  and sec = date.tm_sec
  in Printf.sprintf "%04d-%02d-%02d at %02d:%02d:%02d" year mon day hour min sec

let get () =
  {
    version = Version.version;
    date = date ();
    url = "https://github.com/PedroGasparinho/Cameleer2Html";
    name = "Cameleer2Html"
  }