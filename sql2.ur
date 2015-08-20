structure Self : sig
    val easy_where_rel : tab :: Name -> using ::: {Type} -> notUsing ::: {Type} -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                     -> [using ~ notUsing] => [[tab] ~ otherTables]
	=> folder using
	   -> $(map sql_injectable using) -> (t ::: Type -> sql_binary t t bool)
           -> $using
           -> sql_exp ([tab = using ++ notUsing] ++ otherTables) agg exps bool

    val easy_where_eq : tab :: Name -> using ::: {Type} -> notUsing ::: {Type} -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                     -> [using ~ notUsing] => [[tab] ~ otherTables]
	=> folder using
	   -> $(map sql_injectable using)
           -> $using
           -> sql_exp ([tab = using ++ notUsing] ++ otherTables) agg exps bool    

    val easy_where_gt : tab :: Name -> using ::: {Type} -> notUsing ::: {Type} -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                     -> [using ~ notUsing] => [[tab] ~ otherTables]
	=> folder using
	   -> $(map sql_injectable using)
           -> $using
           -> sql_exp ([tab = using ++ notUsing] ++ otherTables) agg exps bool    

    val easy_where_lt : tab :: Name -> using ::: {Type} -> notUsing ::: {Type} -> otherTables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type}
                     -> [using ~ notUsing] => [[tab] ~ otherTables]
	=> folder using
	   -> $(map sql_injectable using)
           -> $using
           -> sql_exp ([tab = using ++ notUsing] ++ otherTables) agg exps bool    
end = struct
    fun easy_where_rel [tab :: Name] [using] [notUsing] [otherTables] [agg] [exps] [using ~ notUsing] [[tab] ~ otherTables]
		       (fl : folder using)
		       (injs : $(map sql_injectable using))
		       (rels : t ::: Type -> sql_binary t t bool)
		       (r : $using) =
    @foldR2 [sql_injectable] [ident] [fn r => rest :: {Type} -> [notUsing ~ rest] => [notUsing ~ r] => [rest ~ r] => sql_exp ([tab = r ++ rest ++ notUsing] ++ otherTables) agg exps bool]
    (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (inj : sql_injectable t) x
                 (acc : rest :: {Type} -> [notUsing ~ rest] => [notUsing ~ r] => [rest ~ r] => sql_exp ([tab = r ++ rest ++ notUsing] ++ otherTables) agg exps bool)
                 [rest ::_] [notUsing ~ rest] [notUsing ~ ([nm = t] ++ r)] [rest ~ ([nm = t] ++ r)] =>
        (WHERE { sql_binary rels (SQL {{tab}}.{nm}) (SQL {[x]}) } AND {acc [[nm = t] ++ rest]}))
    (fn [rest ::_] [notUsing ~ rest] [notUsing ~ []] [rest ~ []] => (WHERE TRUE)) fl injs r [[]] ! ! !

    
(* NOT ENOUGH OR TOO MUCH POLY ???!? 

(* See monoize.sml
*
*fun monoExp (env, st, fm) (all as (e, loc)) =
*    let
*        val strcat = strcat loc
*        val strcatComma = strcatComma loc
*        fun str s = (L'.EPrim (Prim.String (Prim.Normal, s)), loc)
*        fun strH s = (L'.EPrim (Prim.String (Prim.Html, s)), loc)
*
*        fun poly () =
*            (E.errorAt loc "Unsupported expression";
*             Print.eprefaces' [("Expression", CorePrint.p_exp env all)];
*             (dummyExp, fm))
*)

		...    (rels : $(map (fn t => sql_binary t t bool)  using))
		       (r : $using) =
	(@foldR3 [sql_injectable] [fn t => sql_binary t t bool] [ident] [fn r => rest :: {Type} -> [notUsing ~ rest] => [notUsing ~ r] => [rest ~ r] => sql_exp ([tab = r ++ rest ++ notUsing] ++ otherTables) agg exps bool]
	 (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (inj : sql_injectable t) (rel : sql_binary t t bool) x
                      (acc : rest :: {Type} -> [notUsing ~ rest] => [notUsing ~ r] => [rest ~ r] => sql_exp ([tab = r ++ rest ++ notUsing] ++ otherTables) agg exps bool) =>
          fn [rest :: {Type}] [notUsing ~ rest] [notUsing ~ ([nm = t] ++ r)] [rest ~ ([nm = t] ++ r)] =>
             (WHERE { sql_binary rel (SQL {{tab}}.{nm}) (SQL {[x]}) } AND {acc [[nm = t] ++ rest]}))
	 (fn [rest ::_] [notUsing ~ rest] [notUsing ~ []] [rest ~ []] => (WHERE TRUE)) fl injs rels r  ) [[]] ! ! ! 

*)
	
    fun easy_where_eq [tab :: Name] [using] [notUsing] [otherTables] [agg] [exps] [using ~ notUsing] [[tab] ~ otherTables]
		      (fl : folder using)
		      (injs : $(map sql_injectable using))
      = @@easy_where_rel [tab] [using] [notUsing] [otherTables] [agg] [exps] ! !
	  (fl : folder using)
	  (injs : $(map sql_injectable using))
	  @@sql_eq
	
    fun easy_where_gt [tab :: Name] [using] [notUsing] [otherTables] [agg] [exps] [using ~ notUsing] [[tab] ~ otherTables]
		      (fl : folder using)
		      (injs : $(map sql_injectable using))
      = @@easy_where_rel [tab] [using] [notUsing] [otherTables] [agg] [exps] ! !
	  (fl : folder using)
	  (injs : $(map sql_injectable using))
	  @@sql_gt

    fun easy_where_lt [tab :: Name] [using] [notUsing] [otherTables] [agg] [exps] [using ~ notUsing] [[tab] ~ otherTables]
		      (fl : folder using)
		      (injs : $(map sql_injectable using))
      = @@easy_where_rel [tab] [using] [notUsing] [otherTables] [agg] [exps] ! !
	  (fl : folder using)
	  (injs : $(map sql_injectable using))
	  @@sql_lt	
end   

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
	      
end = struct
		    fun easy_where_eq_gt [tab :: Name] [using] [using1] [notUsing] [otherTables] [agg] [exps]
					 [using ~ using1] [using ++ using1 ~ notUsing] [[tab] ~ otherTables]
					 (fl) (fl1) (injs) (injs1) (r) (r1) =
			(SQL {@@Self.easy_where_eq [tab] [using] [using1 ++ notUsing] [otherTables] [agg] [exps] ! ! fl injs r} AND {@@Self.easy_where_gt [tab] [using1] [using ++ notUsing] [otherTables] [agg] [exps] ! ! fl1 injs1 r1})

		    fun easy_where_eq_lt [tab :: Name] [using] [using1] [notUsing] [otherTables] [agg] [exps]
					 [using ~ using1] [using ++ using1 ~ notUsing] [[tab] ~ otherTables]
					 (fl) (fl1) (injs) (injs1) (r) (r1) =
			(SQL {@@Self.easy_where_eq [tab] [using] [using1 ++ notUsing] [otherTables] [agg] [exps] ! ! fl injs r} AND {@@Self.easy_where_lt [tab] [using1] [using ++ notUsing] [otherTables] [agg] [exps] ! ! fl1 injs1 r1})
			
(* LOL N00B			
		    fun easy_where_eq_gt [tab :: Name] [using] [using1] [notUsing] [otherTables] [agg] [exps]
					 [using ~ using1] [using ++ using1 ~ notUsing] [[tab] ~ otherTables]
					 (fl : folder using) (fl1 : folder using1)
					 (injs : $(map sql_injectable using)) (injs1 : $(map sql_injectable using1))
					 (r : $using) (r1 : $using1) =
			@foldR2 [sql_injectable] [ident]
			 [fn r => rest :: {Type} -> [using ~ r] => [using ++ r ~ rest] => sql_exp ([tab = using ++ r ++ rest] ++ otherTables) agg exps bool]
			 (fn [nm ::_] [t ::_] [r ::_] [[nm] ~ r] (inj1 : sql_injectable t) x
				      (acc : rest :: {Type} -> [using ~ r] => [using ++ r ~ rest] => sql_exp ([tab = using ++ r ++ rest] ++ otherTables) agg exps bool) =>
			  fn [rest ::_] [using ~ ([nm = t] ++ r)] [using ++ ([nm = t] ++ r) ~ rest] =>
			     (WHERE {{tab}}.{nm} < {[x]} AND {acc [[nm = t] ++ rest]}))
			 (fn [rest ::_] [using ~ []] [using ~ rest] => (@Sql.easy_where_eq [tab] ! ! fl injs r) )
			 fl1 injs1 r1 [notUsing] ! !
 *)
			
end
