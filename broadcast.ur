functor Make(M : sig type t
		     val inj : sql_injectable_prim t
		 end) = struct

    type topic = string
    val inj : sql_injectable topic = _

    sequence alarmsIndex
    table alarmsTable : {Index : int, Timestamp : time, Title : string, PublicId : option string, Text : option M.t}

    table listeners : {Title : string, Channel : channel (serialized(bool * option M.t * time))}

    fun create titl pid =
	index <- nextval alarmsIndex;
	nowVal <- now;
	dml (INSERT INTO alarmsTable (Index, Timestamp, Title, PublicId, Text)
	     VALUES ({[index]}, {[nowVal]}, {[titl]}, {[Some pid]}, {[None]}));
	return titl

    fun linked titl =
	r <- oneRow (SELECT * FROM alarmsTable WHERE alarmsTable.Title = {[titl]}
						 AND NOT (alarmsTable.PublicId IS NULL)
					       ORDER BY alarmsTable.Timestamp ASC);
	return (r.AlarmsTable.Title, r.AlarmsTable.PublicId)

    val listed =
	ls <- queryL1 (SELECT alarmsTable.Timestamp, alarmsTable.Title, alarmsTable.PublicId
		 FROM alarmsTable WHERE NOT (alarmsTable.PublicId IS NULL)
				  ORDER BY alarmsTable.Timestamp ASC);
	return (List.mp (fn r => r ++ {Id = r.Title}) ls)
	
    fun subscribers id =
        r <- oneRow (SELECT COUNT( * ) AS N FROM listeners WHERE listeners.Title = {[id]});
        return r.N

    fun subscribe id touched =
	alarmsInitial <- queryL1 (SELECT alarmsTable.Timestamp, alarmsTable.Text
				  FROM alarmsTable
				  WHERE alarmsTable.Title = {[id]}
				    AND alarmsTable.Timestamp <= {[touched]}
				    AND (alarmsTable.PublicId IS NULL)
				  ORDER BY alarmsTable.Timestamp DESC);
        ch <- channel;
        dml (INSERT INTO listeners (Title, Channel) VALUES ({[id]}, {[ch]}));
        return (ch, alarmsInitial)

    fun send id (db, msg) =
	index <- nextval alarmsIndex;
	nowVal <- now;
	(if db then
	     dml (INSERT INTO alarmsTable (Index, Timestamp, Title, PublicId, Text)
		  VALUES ({[index]}, {[nowVal]}, {[id]}, {[None]}, {[msg]}))
	 else return ());
        queryI (SELECT listeners.Channel FROM listeners WHERE listeners.Title = {[id]})
               (fn r => Basis.send r.Listeners.Channel (serialize (db, msg, nowVal)))
end
