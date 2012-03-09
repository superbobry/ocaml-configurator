type t

val of_file  : string -> t
val of_files : string list -> t

val show : t -> string
