structure Alt : sig
    val easy_where_eq_gt : tab :: Name -> using ::: {Type} -> using1 ::: {Type} -> notUsing ::: {Type} -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
			   -> [using ~ using1] => [using ++ using1 ~ notUsing] => [[tab] ~ otherTables]
	=> folder using -> folder using1
	   -> $(map sql_injectable using) -> $(map sql_injectable using1)
           -> $using -> $using1
           -> sql_exp ([tab = using ++ using1 ++ notUsing] ++ otherTables) agg exps bool

    val easy_where_eq_lt : tab :: Name -> using ::: {Type} -> using1 ::: {Type} -> notUsing ::: {Type} -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
			   -> [using ~ using1] => [using ++ using1 ~ notUsing] => [[tab] ~ otherTables]
	=> folder using -> folder using1
	   -> $(map sql_injectable using) -> $(map sql_injectable using1)
           -> $using -> $using1
           -> sql_exp ([tab = using ++ using1 ++ notUsing] ++ otherTables) agg exps bool
	      
end
