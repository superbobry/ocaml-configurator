{
  open Parser

  let keywords = Hashtbl.create 9
  let () = List.iter (fun (kwd, token) -> Hashtbl.add keywords kwd token) [
    ("import", IMPORT);
  ]
}

rule token = parse
  | [' ' '\t' '\r' '\n'] | '#' [^'\n']* {token lexbuf}
  | '=' {EQUALS}
  | ',' {COMMA}
  | '[' {LSB}
  | ']' {RSB}
  | '{' {LCB}
  | '}' {RCB}
  | ['0'-'9']+ as lxm {INT(int_of_string(lxm))}
  | ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '-' '_']* as lxm {
    let lxm = String.lowercase lxm in
    try
      Hashtbl.find keywords lxm
    with Not_found -> IDENT(lxm)
  }
  | '"' ([^'"']+ as lxm) '"' {STRING(lxm)}
  | eof {EOF}
