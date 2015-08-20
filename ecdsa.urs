val generateSecret : unit -> {Secret: string, PublicId: string}
val author : string -> string -> option string
val samePublicIdAuthor : string -> string -> string -> bool
val toLocalStorage : {Secret: string, PublicId: string} -> transaction unit
val fromLocalStorage : unit -> transaction {Secret: string, PublicId: string}
