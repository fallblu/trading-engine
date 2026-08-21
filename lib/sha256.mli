(** SHA-256 digests used to bind audit journals to exact scenario bytes. *)

val digest_string : string -> string
val digest_channel : in_channel -> string
val digest_file : string -> (string, Diagnostic.t) result
