open StdLabels

module Config = struct
  include Map.Make(String)

  let merge =
    let f _key v1 v2 = match (v1, v2) with
      | (None, _) -> v2
      | (_, _)    -> v1
    in merge f
end

type t = Types.value Config.t


(* +----------+
   | Internal |
   +----------+ *)
let rec handle_expr prefix = function
  | `Import fn           -> Config.merge (of_file fn)
  | `Group (name, exprs) ->
      Config.merge (handle_exprs (prefix ^ name ^ ".") exprs)
  | `Bind (key, value)   -> Config.add (prefix ^ key) value

and handle_exprs prefix =
  List.fold_right ~f:(handle_expr prefix) ~init:Config.empty

and string_of_value = function
  | `Bool true  -> "true"
  | `Bool false -> "false"
  | `Int n      -> string_of_int n
  | `Float f    -> string_of_float f
  | `String s   -> s
  | `List vs    ->
      "[" ^ (String.concat ~sep:"," (List.map ~f:string_of_value vs)) ^ "]"

(* +------------+
   | Public API |
   +------------+ *)

and of_file fn =
  let ic = open_in fn in
  let exprs =
    try
      let lexbuf = Lexing.from_channel ic in
      let exprs  = Parser.main Lexer.token lexbuf in
      close_in ic;
      exprs
    with exn ->
      close_in ic;
      raise exn
  in handle_exprs "" exprs

and of_files fns =
  let cfgs = List.map ~f:of_file fns in
  List.fold_right ~f:Config.merge ~init:Config.empty cfgs

and show cfg =
  let bindings = Config.bindings cfg in
  let lines    = List.map
    ~f:(fun (key, value) -> key ^ ": " ^ (string_of_value value))
    bindings
  in String.concat ~sep:"\n" lines
