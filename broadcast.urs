functor Make(M : sig type t
		     val inj : sql_injectable_prim t
		 end) : sig
    type topic
    val inj : sql_injectable topic

    val create : string -> string -> transaction topic
    val linked : string -> transaction (topic * option string)
    val listed : transaction (list {Timestamp : time, Id : topic, Title : string, PublicId : option string})

    val subscribers : topic -> transaction int
    val subscribe : topic -> time -> transaction (channel (serialized (bool * option M.t * time)) * list {Timestamp : time, Text : option M.t})
    val send : topic -> (bool * option M.t) -> transaction unit
end
