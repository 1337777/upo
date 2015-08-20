val setLastVisitMode : transaction unit
val clearLastVisitMode : transaction unit
val o1 : (option int) -> string -> transaction page
val data : string -> int -> int -> int -> int -> int -> int -> transaction page		   
val edit : string -> transaction page		   
val redir : string -> transaction page		   
val listing : unit -> transaction page		   
val main : string -> transaction page

