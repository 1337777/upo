val setIt : id -> xbody -> transaction unit
val getIt : id -> option string
val getValue : id -> option string
val getUrlPrefix : unit -> transaction string
val setUrlPrefix : string -> transaction unit
val windowOpen : string -> transaction unit
val rand : unit -> transaction int
val transformxml : list string -> int -> transaction bool
