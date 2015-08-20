val netHost = "localhost:8081"
val orgHost = "127.0.0.1:8082"
val netPrefix = "http://" ^ netHost ^ "/o1/"
val orgPrefix = "http://" ^ orgHost ^ "/"

open Bootstrap3

type permission = {Add : bool, TopAdd : bool, Delete : bool, Modify : bool}

functor Make(M : sig
		 con const :: {Type}
                 con givenEq :: {Type}
                 con givenLt :: {Type}
		 type key
		 type keyWidget
		 con index :: Name
                 con parent :: Name
		 con parent2 :: Name
		 type timeurnValue = string
		 type altPublicIdValue = option string
		 type authValue = option string
                 con fs :: {(Type * Type)}
		 constraint const ~ givenEq
                 constraint (const ++ givenEq) ~ givenLt
		 constraint (const ++ givenEq) ~ [Channel] (* extra const .. *)
		 constraint (const ++ givenEq) ~ [Root]
		 constraint (const ++ givenEq ++ givenLt) ~ [index = key]
		 constraint (const ++ givenEq ++ givenLt ++ [index = key]) ~ [parent = option key]
		 constraint (const ++ givenEq ++ givenLt ++ [index = key] ++ [parent = option key]) ~ [parent2 = option key]
		 constraint (const ++ givenEq ++ givenLt ++ [index = key] ++ [parent = option key] ++ [parent2 = option key]) ~ ([Timeurn = timeurnValue] ++ [AltPublicId = altPublicIdValue] ++ [Auth = authValue])
		 constraint (const ++ givenEq ++ givenLt ++ [index = key] ++ [parent = option key] ++ [parent2 = option key] ++ [Timeurn = timeurnValue] ++ [AltPublicId = altPublicIdValue] ++ [Auth = authValue]) ~ fs
		 val const : $const
		 val publicId : $givenEq -> signal string
		 val givenEqFromPublicId : string -> transaction $givenEq
		 val autoGivenLt : $(map transaction givenLt) (* example: now, timestamp *)
		 val autoIndex : transaction key (* example: nextval *)
		 val key_widget : Widget.t key keyWidget
		 val widgets : $(map Widget.t' fs)

                 table tab : $(const ++ givenEq ++ givenLt ++ [index = key, parent = option key] ++ [Timeurn = timeurnValue] ++ [AltPublicId = altPublicIdValue] ++ [Auth = authValue] ++ [parent2 = option key] ++ map fst fs)
			     (*contraint index unique? contraint parent foreign key on delete cascade ?*)

		 val constFl : folder const
                 val givenEqFl : folder givenEq
		 val givenLtFl : folder givenLt
                 val fl : folder fs
			  
		 val key_eq : eq key
		 val key_show : show key
		 val givenLtOrd : ord $givenLt
			    
		 val constInj : $(map sql_injectable const)
		 val givenEqInj : $(map sql_injectable givenEq)
		 val givenLtInj : $(map sql_injectable givenLt)
		 val key_inj : sql_injectable_prim key
                 val injs : $(map sql_injectable (map fst fs))

                 val labels : $(map (fn _ => string) fs)
                 val permission : transaction permission
		 val givenEqEq : eq $givenEq
		 val amGivenEq : transaction (option $givenEq)

                 val onAdd : $(givenLt ++ [index = key, parent = option key] ++ [Timeurn = timeurnValue] ++ [AltPublicId = altPublicIdValue] ++ [Auth = authValue] ++ [parent2 = option key] ++ map fst fs) -> transaction unit
                 val onDelete : key -> transaction unit
                 val onModify : {Old : key, New : $(givenLt ++ [index = key, parent = option key] ++ [Timeurn = timeurnValue] ++ [AltPublicId = altPublicIdValue] ++ [Auth = authValue] ++ [parent2 = option key] ++ map fst fs)} -> transaction unit
             end) = struct

    open M
	 
    type input =  (option key) * $(M.givenEq) * $(M.givenLt) * $(M.givenLt)

    type timeurnValue = string
    type timeurnWidget = TimeurnStringBox.timeurnStringBox
    type altPublicIdValue = option string
    type altPublicIdWidget = OptionBox.optionBoxWidget Widget.textbox (* transparent *)
    type authValue = option string
    type authWidget = AuthBox.authBoxWidget (* transparent *)

    con relDataCons = [Timeurn = timeurnValue] ++ [AltPublicId = altPublicIdValue] ++ [Auth = authValue]
    val relDataConsFl : folder relDataCons = _
    val relDataConsInjs : $(map sql_injectable relDataCons) = {Timeurn = _} ++ {AltPublicId = _} ++ {Auth = _}
    con whereDataCons = [index = key, parent = option key] ++ relDataCons ++ [parent2 = option key] ++ map fst fs
    con extraDataCons = givenLt ++ [index = key, parent = option key] ++ relDataCons ++ [parent2 = option key]
    con dataCons = (extraDataCons ++ map fst fs)
    type data = $dataCons
    con extraStateCons = [Timeurn = timeurnWidget] ++ [AltPublicId = altPublicIdWidget] ++ [Auth = authWidget]
    type state = $(extraStateCons ++ (map snd fs))

    datatype action =
             ADD of data
           | DEL of key
           | MOD of { Old : key, New : data }

    table listeners : $([Root = option key] ++ givenEq ++ [Channel = channel action])

    type row = { Editing : source (option (state * state)),
                 Content : data }

    type a = { Root : option key,
	       GivenEq : $givenEq,
	       GivenLt : $givenLt,
	       GivenLtLimit : $givenLt,
	       Perm : permission,
               Rows : source (list row),
	       SecretStorage : {Secret : source string, PublicId : source string},
               ToAdd : state,
               Channel : channel action }

    val freshRow = @Monad.mapR _ [Widget.t'] [snd]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.create w) fl widgets

    val initRow = @Monad.mapR2 _ [Widget.t'] [fst] [snd]
                    (fn [nm ::_] [p ::_] (w : Widget.t' p) => @Widget.initialize w)
                    fl widgets

    val rowOut = @Monad.mapR2 _ [Widget.t'] [snd] [fst]
                  (fn [nm ::_] [p ::_] (w : Widget.t' p) (v : snd p) =>
                      current (@Widget.value w v))
                  fl widgets

    fun create root givenEq givenLt givenLtLimit : transaction a =
	perm <- permission;
	secretStorage <- Monad.exec {Secret = source "", PublicId = source ""};
	toAdd <- freshRow;
	toAddTimeurn <- @Widget.create TimeurnStringBox.timeurnStringBox;
	toAddAltPublicId <- @Widget.create (@OptionBox.optionBox Widget.textbox);
	toAddAuth <- @Widget.create AuthBox.authBox;
	set toAddAuth.Timeurn (Some (@Widget.value TimeurnStringBox.timeurnStringBox toAddTimeurn));
	set toAddAuth.PublicId (Some (publicId (givenEq)));
	set toAddAuth.AltPublicId (Some (@Widget.value (@OptionBox.optionBox Widget.textbox) toAddAltPublicId));
	set toAddAuth.Paste (Some secretStorage.Secret);

	rows <- (
	let
	    fun recurse root =
		initialList <- (case root of
				    None => return List.Nil
				  | Some index => List.mapQueryM (SELECT tab.{{ dataCons }}
								  FROM tab
								  WHERE tab.{index} = {[index]} AND
								    { @@Sql2.Alt.easy_where_eq_gt [#Tab] [const ++ givenEq] [givenLt] [ whereDataCons ] [[]] [[]] [[]] ! ! ! (@Folder.concat ! constFl givenEqFl) (givenLtFl) (constInj ++ givenEqInj) (givenLtInj) (const ++ givenEq) (givenLt) } AND
								    { @@Sql2.Alt.easy_where_eq_lt [#Tab] [const ++ givenEq] [givenLt] [ whereDataCons ] [[]] [[]] [[]] ! ! ! (@Folder.concat ! constFl givenEqFl) (givenLtFl) (constInj ++ givenEqInj) (givenLtInj) (const ++ givenEq) (givenLtLimit) }
								  ORDER BY {{{ @Sql.order_by givenLtFl (@Sql.some_fields [#Tab] [givenLt] ! ! givenLtFl) sql_desc }}}
								  (* TODO: ? LIMIT 1 *)
								 (* order by necessary? for (initial and after addition of rows) rendering , list is sorted again along givenLt *) )
								 (fn r =>
								     editing <- source None;
								     return ({Editing = editing,
									     Content = r.Tab} : row)));
		children <- List.mapQueryM (SELECT tab.{{ dataCons }}
					    FROM tab
					    WHERE { Top.eqNullable' (SQL tab.{parent}) root } AND
					    { @@Sql2.Alt.easy_where_eq_gt [#Tab] [const ++ givenEq] [givenLt] [ whereDataCons ] [[]] [[]] [[]] ! ! ! (@Folder.concat ! constFl givenEqFl) (givenLtFl) (constInj ++ givenEqInj) (givenLtInj) (const ++ givenEq) (givenLt) } AND
					    { @@Sql2.Alt.easy_where_eq_lt [#Tab] [const ++ givenEq] [givenLt] [ whereDataCons ] [[]] [[]] [[]] ! ! ! (@Folder.concat ! constFl givenEqFl) (givenLtFl) (constInj ++ givenEqInj) (givenLtInj) (const ++ givenEq) (givenLtLimit) }
								     ORDER BY {{{ @Sql.order_by givenLtFl (@Sql.some_fields [#Tab] [givenLt] ! ! givenLtFl) sql_desc }}}
					   (* order by necessary? for (initial and after addition of rows) rendering , list is sorted again along givenLt *) )
					   (fn r =>
					       editing <- source None;
					       return ({Editing = editing,
						       Content = r.Tab} : row));
		(List.foldlM (fn r acc => descendants <- recurse (Some r.Content.index); return (List.append descendants acc)) initialList children)
	in
	    recurse root
	end);
	(* creation List.sort ? *)
	rows <- source rows;

        chan <- channel;
	@@Sql.easy_insert [[Root = _] ++ givenEq ++ [Channel = _]] [_] ({Root = _} ++ givenEqInj ++ {Channel = _})
	 (@Folder.concat ! (@Folder.cons [#Root] [option key] ! Folder.nil) (@Folder.concat ! givenEqFl (@Folder.cons [#Channel] [channel action] ! Folder.nil)))
	 listeners
	 ({Root = root} ++ givenEq ++ {Channel = chan});
        (* dml (INSERT INTO listeners(GivenEq, Channel) VALUES ({[givenEq]}, {[chan]})); *)
                                   
        return { Root = root,
		 GivenEq = givenEq,
		 GivenLt = givenLt,
		 GivenLtLimit = givenLtLimit,
		 Perm = perm,
                 Rows = rows,
		 SecretStorage = secretStorage,
                 ToAdd = {Timeurn = toAddTimeurn} ++ {AltPublicId = toAddAltPublicId} ++ {Auth = toAddAuth} ++ toAdd,
                 Channel = chan }


    fun onload (a : a) =
        let
            fun loop () =
                msg <- recv a.Channel;
                (case msg of
                     ADD d =>
                     rows <- get a.Rows;
                     editing <- source None;
                     set a.Rows ( ({Editing = editing, Content = d} :: rows) )
                   | DEL d =>
                     rows <- get a.Rows;
		     (* recursive delete?, method of rendering make recursive delete not necessary
                      * set a.Rows (List.filter (fn a => a.Content.index <> d) rows) *)
		     let
			 fun recurse (root : key) rws =
			     let
				 val rws0 = List.foldl (fn r acc => recurse r.Content.index acc ) rws (List.filter (fn r => r.Content.parent = Some root) rws (*rows?*))
			     in
				 List.filter (fn r => r.Content.index <> root) rws0
			     end
		     in
			 set a.Rows ( (recurse d rows) )
		     end
                   | MOD d =>
                     rows <- get a.Rows;
                     editing <- source None;
                     set a.Rows ( (List.mp (fn a => if a.Content.index = d.Old then
								 {Editing = editing, Content = d.New}
							     else
								 a) rows) )
		);  (* too much order-by-sorting / creation-sorting / channel-sorting / rendering-sorting confusion ? *)
                loop ()
        in
            spawn (loop ())
        end
	
    fun ensure gv =
        user <- amGivenEq;
        case user of
            None => return () (* NO EXPECTED INPUT *) (* error <xml>Must be authenticated to access this page</xml> *)
          | Some user =>
            if user = gv then
                return ()
            else
                return () (* NO EXPECTED INPUT *) (* error <xml>Wrong user to be accessing this page</xml> *)

    fun add root givenEq (r : $([parent = option key] ++ relDataCons ++ [parent2 = option key] ++ map fst fs))  =
        perm <- permission;
        (if perm.Add then
             return ()
         else
             error <xml>Don't have permission to add row</xml>);
	ensure givenEq;
	
	index <- autoIndex;
	givenLt <- @Monad.exec _ autoGivenLt givenLtFl;
	
        @@Sql.easy_insert [const ++ givenEq ++ dataCons] [_]
	  (constInj ++ givenEqInj ++ givenLtInj ++ {index = @sql_prim key_inj} ++ {parent = @sql_option_prim key_inj} ++ relDataConsInjs ++ {parent2 = @sql_option_prim key_inj} ++ injs)
	  (@Folder.concat ! constFl
	    (@Folder.concat ! givenEqFl
	      (@Folder.concat ! givenLtFl
		(@Folder.concat ! (@Folder.cons [parent] [option key] ! (@Folder.cons [index] [key] ! Folder.nil))
		  (@Folder.concat ! relDataConsFl
		    (@Folder.concat ! (@Folder.cons [parent2] [option key] ! Folder.nil)
		    (@@Folder.mp [fst] [_] fl)))))))
	  tab (const ++ givenEq ++ givenLt ++ {index = index} ++ r) ;

        queryI1 (SELECT * FROM listeners WHERE
					   { @Sql.easy_where [#Listeners] ! ! givenEqInj givenEqFl givenEq } AND
					   { Top.eqNullable' (SQL listeners.Root) root })
        (fn x => send x.Channel (ADD (givenLt ++ {index = index} ++ r)));

        onAdd (givenLt ++ {index = index} ++ r)

    fun del root givenEq (index : key) =
        perm <- permission;
        (if perm.Delete then
             return ()
         else
             error <xml>Don't have permission to delete row</xml>);
	ensure givenEq;

        dml (DELETE FROM tab
             WHERE T.{index} = {[index]});

        queryI1 (SELECT * FROM listeners WHERE
					   { @Sql.easy_where [#Listeners] ! ! givenEqInj givenEqFl givenEq } AND
					   { Top.eqNullable' (SQL listeners.Root) root })
        (fn x => send x.Channel (DEL index));

        onDelete index

	
    fun mod root givenEq (r : { Old : key, New : $([parent = option key] ++ relDataCons ++ [parent2 = option key] ++ map fst fs) }) =
	(* todo: really possible to update parent? *)

        perm <- permission;
        (if perm.Modify then
             return ()
         else
             error <xml>Don't have permission to delete row</xml>);
	ensure givenEq;

	(* really update givenLt (the timestamp) or keep the old then use easy_update''' with givenLt as untouched otherFields ? *)
	givenLt <- @Monad.exec _ autoGivenLt givenLtFl;

	@@Sql.easy_update'' [[index = key]]
	  [givenLt ++ [parent = option key] ++ relDataCons ++ [parent2 = option key] ++ map fst fs] [_] [_] ! !
	  {index = @sql_prim key_inj}
	  (givenLtInj ++ {parent = @sql_option_prim key_inj} ++ relDataConsInjs ++ {parent2 = @sql_option_prim key_inj} ++ injs)
	  (@Folder.cons [index] [key] ! Folder.nil)
	  (@Folder.concat ! givenLtFl
	    (@Folder.concat ! (@Folder.cons [parent] [option key] ! (Folder.nil))
	      (@Folder.concat ! relDataConsFl
		(@Folder.concat ! (@Folder.cons [parent2] [option key] ! Folder.nil)
		(@@Folder.mp [fst] [_] fl)))))
	  tab {index = r.Old} (r.New ++ givenLt);
	
        queryI1 (SELECT * FROM listeners WHERE
					   { @Sql.easy_where [#Listeners] ! ! givenEqInj givenEqFl givenEq } AND
					   { Top.eqNullable' (SQL listeners.Root) root })
        (fn x => send x.Channel (MOD {Old = r.Old, New = (givenLt ++ {index = r.Old} ++ r.New)}));

        onModify {Old = r.Old, New = (givenLt ++ {index = r.Old} ++ r.New)}

	
    fun render (ctx : Ui.context) (a : a) = ( <xml>
      <table class="bs3-table">
        <tr><th/>
	  <th>Timeurn</th><th>AltPublicId</th><th>Auth</th>
          { @mapX [fn _ => string] [tr]
             (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r] (s : string) => <xml><th>{[s]}</th></xml>)
             fl labels } </tr>
	  
          <dyn signal={rs <- signal a.Rows;
		       let
			   fun recurse (root : option key) depth =
			       List.mapXM (fn r => children <- recurse (Some r.Content.index) (depth+1);
					      return ( <xml>
							<dyn signal={ed <- signal r.Editing;
								     return (case ed of
										 None => ( <xml><tr style={ Basis.oneProperty Basis.noStyle (Basis.value (Basis.property "background-color") (Basis.atom (case depth of 0 => "white" | 1 => "lightgrey" | 2 => "darkgrey" | _ => "grey") )) } >
										   <td>
										   {if a.Perm.Delete then
											( Ui.modalButton ctx
												       (CLASS "btn glyphicon glyphicon-remove")
												       ( <xml/> )
												       (return (Ui.modal
												       (rpc (del a.Root a.GivenEq r.Content.index))
												       ( <xml>Are you sure you want to delete this row?</xml> )
												       ( <xml/> )
												       ( <xml>Yes!</xml> ) )) )
												       else
												       ( <xml/> ) }
										   {if a.Perm.Modify then
											( <xml><button class="btn glyphicon glyphicon-pencil" onclick={fn _ => fr <- initRow (r.Content --- extraDataCons);
																			  frTimeurn <- @Widget.initialize TimeurnStringBox.timeurnStringBox r.Content.Timeurn;
																			  frAltPublicId <- @Widget.initialize (@OptionBox.optionBox Widget.textbox) r.Content.AltPublicId;
																			  frAuth <- @Widget.initialize AuthBox.authBox r.Content.Auth;
																			  set frAuth.Timeurn (Some (@Widget.value TimeurnStringBox.timeurnStringBox frTimeurn));
																			  set frAuth.PublicId (Some (publicId (a.GivenEq)));
																			  set frAuth.AltPublicId (Some (@Widget.value (@OptionBox.optionBox Widget.textbox) frAltPublicId));
																			  set frAuth.Paste (Some a.SecretStorage.Secret);
																			  
																			  fr2 <- freshRow;
																			  fr2Timeurn <- @Widget.create TimeurnStringBox.timeurnStringBox;
																			  fr2AltPublicId <- @Widget.create (@OptionBox.optionBox Widget.textbox);
																			  fr2Auth <- @Widget.create AuthBox.authBox;
																			  set fr2Auth.Timeurn (Some (@Widget.value TimeurnStringBox.timeurnStringBox fr2Timeurn));
																			  set fr2Auth.PublicId (Some (publicId (a.GivenEq)));
																			  set fr2Auth.AltPublicId (Some (@Widget.value (@OptionBox.optionBox Widget.textbox) fr2AltPublicId));
																			  
																			  set fr2Auth.Paste (Some a.SecretStorage.Secret);
																			  
																			  set r.Editing (Some ({Timeurn = frTimeurn} ++ {AltPublicId = frAltPublicId} ++ {Auth = frAuth} ++ fr,
																					       {Timeurn = fr2Timeurn} ++ {AltPublicId = fr2AltPublicId} ++ {Auth = fr2Auth} ++ fr2))} />
											   </xml> ) else
										    ( <xml/> ) }
										   { Ui.modalButton ctx (CLASS "btn btn-primary") (<xml>Merge:</xml>)
												    (targetRoot <- @Widget.create (@OptionBox.optionBox key_widget);
												     targetPublicId <- source "";
												     redirectSwitch <- source True;
												     frAuth <- @Widget.create AuthBox.authBox;
												     frAltPublicId <- @Widget.initialize (@OptionBox.optionBox Widget.textbox) None;
												     set frAuth.Timeurn (Some (return r.Content.Timeurn));
												     set frAuth.PublicId (Some (signal targetPublicId));
												     set frAuth.AltPublicId (Some (@Widget.value (@OptionBox.optionBox Widget.textbox) frAltPublicId));
												     set frAuth.Paste (Some a.SecretStorage.Secret);
												     return (Ui.modal (vsTargetRoot <- current (@Widget.value (@OptionBox.optionBox key_widget) targetRoot);
														       vsTargetPublicId <- current (signal targetPublicId);
														       redirectSwitchVal <- current (signal redirectSwitch);
														       vsAuth <- current (@Widget.value AuthBox.authBox frAuth);
														       (* TODO: pretty FAIL if vsAuth is KO *)
														       case vsAuth of
															   None => alert "Error, auth fail"
															 | _ =>
															   givenEqFromPublicIdvsTargetPublicId <- (givenEqFromPublicId vsTargetPublicId);
															   rpc (add vsTargetRoot (givenEqFromPublicIdvsTargetPublicId)
																    ({parent = vsTargetRoot}
																	 ++ {Timeurn = r.Content.Timeurn} ++ {AltPublicId = None} ++ {Auth = vsAuth}
																	 ++ {parent2 = Some r.Content.index}
																	 ++ (r.Content --- extraDataCons)) );
															   if Top.not redirectSwitchVal then return ()
															   else (redirect (bless ("http://localhost:8081/EditableTree/o1/" ^ (case vsTargetRoot of None => "None" | Some index => "Some/" ^ show index) ^ "/" ^ (show vsTargetPublicId)))) )
														      (<xml>Merge</xml>)
														      (<xml>targetRoot {@Widget.asWidget (@OptionBox.optionBox key_widget) targetRoot}, targetPublicId <ctextbox source={targetPublicId}/>, Auth  {@Widget.asWidget AuthBox.authBox frAuth}
															<p>redirectSwitch? <ccheckbox source={redirectSwitch}/></p></xml>)
														      (<xml>OK MERGE</xml>))) }</td>
										   <td>{@Widget.asValue TimeurnStringBox.timeurnStringBox r.Content.Timeurn}
										     { Ui.modalButton ctx
												       (Basis.null)
												       ( <xml>Json</xml> )
												       (return (Ui.modal
												       (return ())
												       ( <xml>{[r.Content.Timeurn]}</xml> )
												       ( <xml/> )
												       ( <xml>OK</xml> ))) }</td>
										     <td><div style="overflow-x: auto; max-width: 200px;">
										       {@Widget.asValue (@OptionBox.optionBox Widget.textbox) r.Content.AltPublicId}</div></td>
										   <td>{@Widget.asValue AuthBox.authBox r.Content.Auth}</td>
										   { @mapX2 [Widget.t'] [fst] [_]
										      (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
												   (w : Widget.t' p) (v : fst p) =>
											  ( <xml><td>{@Widget.asValue w v}</td></xml> ) )
										      fl widgets (r.Content --- extraDataCons ) }
										   </tr></xml> )
										   
										   | Some (ws, wsToAdd) => ( <xml>
										     <tr style={ Basis.oneProperty Basis.noStyle (Basis.value (Basis.property "background-color") (Basis.atom (case depth of 0 => "white" | 1 => "lightgrey" | 2 => "darkgrey" | _ => "grey") )) }>
										     <td>
										       <button class="btn glyphicon glyphicon-ok" onclick={fn _ => vs <- rowOut (ws --- extraStateCons);
																	      vsTimeurn <- current (@Widget.value TimeurnStringBox.timeurnStringBox ws.Timeurn);
																	      vsAltPublicId <- current (@Widget.value (@OptionBox.optionBox Widget.textbox) ws.AltPublicId);
																	      vsAuth <- current (@Widget.value AuthBox.authBox ws.Auth);
																	      set r.Editing None;
																	      rpc (mod a.Root a.GivenEq {Old = r.Content.index,
																				  New = {parent = r.Content.parent (*maybe server-side parent verif here*) } ++ {Timeurn = vsTimeurn} ++ {AltPublicId = vsAltPublicId} ++ {Auth = vsAuth} ++ {parent2 = r.Content.parent2} ++ vs})}
																	      />
																	      <button class="btn glyphicon glyphicon-remove" onclick={fn _ => set r.Editing None}/>
										       </td>
										       <td>{@Widget.asWidget TimeurnStringBox.timeurnStringBox ws.Timeurn}
											 { Ui.modalButton ctx
													  (Basis.null)
													  ( <xml>Json</xml> )
													  (return (Ui.modal
														       (return ())
														       ( <xml><active code={json <- current (@Widget.value TimeurnStringBox.timeurnStringBox ws.Timeurn); return (<xml>{[json]}</xml>)}/></xml> )
														       ( <xml/> )
														       ( <xml>OK</xml> ))) }</td>
											 <td>{@Widget.asWidget (@OptionBox.optionBox Widget.textbox) ws.AltPublicId}</td>
											 <td>{@Widget.asWidget AuthBox.authBox ws.Auth}</td>
										       { @mapX2 [Widget.t'] [snd] [_]
											  (fn [nm ::_] [p ::_] [r ::_] [[nm] ~ r]
												       (w : Widget.t' p) (v : snd p) =>
											      ( <xml><td>{@Widget.asWidget w v}</td></xml> ) )
											  fl widgets (ws --- extraStateCons) }
										     </tr>
										     
										     { if a.Perm.Add then
											   ( <xml>
											     <tr style={ Basis.oneProperty Basis.noStyle (Basis.value (Basis.property "background-color") (Basis.atom (case depth of 0 => "white" | 1 => "lightgrey" | 2 => "darkgrey" | _ => "grey") )) }>
                          <th><button value="Add:" class="btn btn-primary"
	                      onclick={fn _ => vs <- rowOut (wsToAdd --- extraStateCons);
					  vsTimeurn <- current (@Widget.value TimeurnStringBox.timeurnStringBox wsToAdd.Timeurn);
					  vsAltPublicId <- current (@Widget.value (@OptionBox.optionBox Widget.textbox) wsToAdd.AltPublicId);
					  vsAuth <- current (@Widget.value AuthBox.authBox wsToAdd.Auth);
					  set r.Editing None;
					  rpc (add a.Root a.GivenEq ({parent = Some r.Content.index} ++ {Timeurn = vsTimeurn} ++ {AltPublicId = vsAltPublicId} ++ {Auth = vsAuth} ++ {parent2 = None} ++ vs))
				      } />
                           { Ui.modalButton ctx (CLASS "btn btn-primary") (<xml>Branch:</xml>)
					    (vs <- rowOut (wsToAdd --- extraStateCons);
					     vsTimeurn <- current (@Widget.value TimeurnStringBox.timeurnStringBox wsToAdd.Timeurn);
					     vsAltPublicId <- current (@Widget.value (@OptionBox.optionBox Widget.textbox) wsToAdd.AltPublicId);
					     vsAuth <- current (@Widget.value AuthBox.authBox wsToAdd.Auth);
					     case vsAltPublicId of
						 None => return (Ui.modal (return ()) (<xml>Error</xml>) (<xml>Branching requires some target alt public id</xml>) (<xml>OK</xml>))
					       | Some vsAltPublicId => 
						 targetGivenEq <- givenEqFromPublicId vsAltPublicId;
						 set r.Editing None;
						 rpc (add (Some r.Content.index) targetGivenEq ({parent = Some r.Content.index} ++ {Timeurn = vsTimeurn} ++ {AltPublicId = None} ++ {Auth = vsAuth} ++ {parent2 = None} ++ vs));
						 publicIdtargetGivenEq <- current ((publicId targetGivenEq));
						 return (Ui.modal (redirect (bless ("http://localhost:8081/EditableTree/o1/Some/" ^ (show r.Content.index) ^ "/" ^ show (publicIdtargetGivenEq))))
							  (<xml>Success</xml>) (<xml>Request sent to server</xml>) (<xml>Redirect?</xml>)) )
			    }</th>
												 <td>{@Widget.asWidget TimeurnStringBox.timeurnStringBox wsToAdd.Timeurn}
												   { Ui.modalButton ctx
														    (Basis.null)
														    ( <xml>Json</xml> )
														    (timeurnVal <- current (@Widget.value TimeurnStringBox.timeurnStringBox wsToAdd.Timeurn);
														     return (Ui.modal
																 (return ())
																 ( <xml>{[timeurnVal]}</xml> )
																 ( <xml/> )
																 ( <xml>OK</xml> ))) }</td>
												 <td>{@Widget.asWidget (@OptionBox.optionBox Widget.textbox) wsToAdd.AltPublicId}</td>
												 <td>{@Widget.asWidget AuthBox.authBox wsToAdd.Auth}</td>
												 { @mapX2 [Widget.t'] [snd] [tr]
												    (fn [nm ::_] [p :: (Type * Type)] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : snd p) =>
													<xml><td>{@Widget.asWidget w v}</td></xml>)
												    fl widgets (wsToAdd --- extraStateCons) }
											       </tr>
											     </xml> )
										       else
											   ( <xml/> ) }
										     </xml> ) )
								    } />
							  { children }
										     </xml> ) )
					  ( List.sort (fn r1 r2 => r1.Content --- whereDataCons > r2.Content --- whereDataCons)
						      ( List.filter (fn r => r.Content.parent = root)
								    ( rs ) ) )
		       in
			   recurse a.Root 0
		       end }/>
			    
			    { if a.Perm.TopAdd || a.Perm.Add then
				  ( <xml>
				    <tr>
				      <th><button value="Add:" class="btn btn-primary" onclick={fn _ => vs <- rowOut (a.ToAdd --- extraStateCons);
								    vsTimeurn <- current (@Widget.value TimeurnStringBox.timeurnStringBox a.ToAdd.Timeurn);
								    vsAltPublicId <- current (@Widget.value (@OptionBox.optionBox Widget.textbox) a.ToAdd.AltPublicId);
								    vsAuth <- current (@Widget.value AuthBox.authBox a.ToAdd.Auth);
								    rpc (add a.Root a.GivenEq ({parent = a.Root} ++ {Timeurn = vsTimeurn} ++ {AltPublicId = vsAltPublicId} ++ {Auth = vsAuth} ++ {parent2 = None} ++ vs)) } />
                                          { Ui.modalButton ctx (CLASS "btn btn-primary") (<xml>Branch:</xml>)
					    (vs <- rowOut (a.ToAdd --- extraStateCons);
					     vsTimeurn <- current (@Widget.value TimeurnStringBox.timeurnStringBox a.ToAdd.Timeurn);
					     vsAltPublicId <- current (@Widget.value (@OptionBox.optionBox Widget.textbox) a.ToAdd.AltPublicId);
					     vsAuth <- current (@Widget.value AuthBox.authBox a.ToAdd.Auth);
					     case vsAltPublicId of
						 None => return (Ui.modal (return ()) (<xml>Error</xml>) (<xml>Branching requires some target alt public id</xml>) (<xml>OK</xml>))
					       | Some vsAltPublicId => 
						 targetGivenEq <- givenEqFromPublicId vsAltPublicId;
						 rpc (add a.Root targetGivenEq ({parent = a.Root} ++ {Timeurn = vsTimeurn} ++ {AltPublicId = None} ++ {Auth = vsAuth} ++ {parent2 = None} ++ vs));
						 publicIdtargetGivenEq <- current ((publicId targetGivenEq));
						 return (Ui.modal (redirect (bless ("http://localhost:8081/EditableTree/o1/" ^ (case a.Root of None => "None" | Some index => "Some/" ^ show index) ^ "/" ^ show (publicIdtargetGivenEq))))
							  (<xml>Success</xml>) (<xml>Request sent to server</xml>) (<xml>Redirect?</xml>)) )
					   }</th>
					<td>{@Widget.asWidget TimeurnStringBox.timeurnStringBox a.ToAdd.Timeurn}
					  { Ui.modalButton ctx
							   (Basis.null)
							   ( <xml>Json</xml> )
							   (return (Ui.modal
									(return ())
									( <xml><active code={json <- current (@Widget.value TimeurnStringBox.timeurnStringBox a.ToAdd.Timeurn); return (<xml>{[json]}</xml>)}/></xml> )
									( <xml/> )
									( <xml>OK</xml> ))) }</td>
					<td>{@Widget.asWidget (@OptionBox.optionBox Widget.textbox) a.ToAdd.AltPublicId}</td>
					<td>{@Widget.asWidget AuthBox.authBox a.ToAdd.Auth}</td>
					{ @mapX2 [Widget.t'] [snd] [tr]
					   (fn [nm ::_] [p :: (Type * Type)] [r ::_] [[nm] ~ r] (w : Widget.t' p) (v : snd p) =>
					       <xml><td>{@Widget.asWidget w v}</td></xml>)
					   fl widgets (a.ToAdd --- extraStateCons) }
				      </tr>
				    </xml> )
			      else
				  ( <xml/> ) }
			    </table>
			    <hr/><div>Local Storage: Secret <ctextbox source={a.SecretStorage.Secret} />, PublicId <ctextbox source={a.SecretStorage.PublicId} />,
			      <button onclick={fn _ => @Monad.appR2 _ [source] [ident]
							(fn [nm ::_] [t ::_ ] => set)
							(_ : folder [Secret = _, PublicId = _])
							a.SecretStorage (Ecdsa.generateSecret())}>Generate</button>
			      <button onclick={fn _ => Basis.bind
							   (Monad.exec
								(@@Top.mp [source] [transaction] (fn [t :::_] => signal >>> current) [[Secret = _, PublicId = _]] _ a.SecretStorage))
							   Ecdsa.toLocalStorage }>toLocalStorage</button>
			      <button onclick={fn _ => Basis.bind (Ecdsa.fromLocalStorage())
								  (@Monad.appR2 _ [source] [ident]
								    (fn [nm ::_] [t ::_ ] => set)
								    (_ : folder [Secret = _, PublicId = _])
								    a.SecretStorage) }>fromLocalStorage</button></div>
			      <hr/><div>urlPrefix: <active code={urlPrefix <- source "";
							    ls <- return ("???777???" :: orgPrefix :: "http://google.com/?q=" :: "https://en.wikipedia.org/wiki/" :: "http://mega.nz/" :: "http://www.baidu.com/s?wd=" :: [] );
							    return (<xml>
							      <cselect source={urlPrefix}>{ List.mapX (fn s => <xml><coption Value={s}>{[s]}</coption></xml>) ls }</cselect> 
							      <ctextbox source={urlPrefix}/> 
							      <button onclick={fn _ => urlPrefixVal <- current (signal urlPrefix);
										  urlPrefixVal <- (if urlPrefixVal = "???777???"  then
												       randVal <- SetInner.rand();
												       randVal <- return (Basis.mod randVal 4 + 1);
												       urlPrefixValO <- return (List.nth ls randVal);
												       case urlPrefixValO of
													   None => return urlPrefixVal (* error ... *)
													 | Some urlPrefixVal => return urlPrefixVal
												   else return (urlPrefixVal));						      
										  SetInner.setUrlPrefix urlPrefixVal}>setUrlPrefix</button></xml>)}/></div> </xml> )
					    
    val ui (input : input) : Ui.t a = {Create = create input.1 input.2 input.3 input.4,
				      Onload = onload,
				      Render = render}
					    
end

sequence editableTreeInstanceSequence
table editableTreeInstanceTable : { Const : int, GivenEq : string, Index : int, GivenLt : time, Parent : option int, Timeurn : string, AltPublicId : option string, Auth : option string, Parent2 : option int, Geolocation : string }
		  CONSTRAINT IndexConstraint UNIQUE Index,
		  CONSTRAINT ParentConstraint FOREIGN KEY Parent REFERENCES editableTreeInstanceTable (Index) ON DELETE CASCADE ON UPDATE CASCADE

val givenEq_top : string = "049d5492bf0a3942b8a051a11abf3a11c8ffaf21a94c95c80ee28a0cb4b59b97b5c15d1fbb2dc9ff17242f7bbd82f26be2804acb33932b6220a8786330730cae6f" 
(* secret 09cf9b4df469d4dcb1ddae4d512e3a89fdefcb4ed833f988d482f9a0fe97a1c4	*)
			   
structure EditableTreeInstance = Make ( struct
			      con const = [Const = int]
			      con givenEq = [GivenEq = string]
			      con givenLt = [GivenLt = time]
			      type keyWidget = Widget.intbox
			      con index = #Index
			      con parent = #Parent
			      con parent2 = #Parent2
			      con fs = [ Geolocation = (_, GeolocationBox.geolocationBox) ]
			      val const = {Const = 101}
			      fun publicId (ge : $givenEq) = return ge.GivenEq
			      fun givenEqFromPublicId (pid : string) = return {GivenEq = pid}
			      val autoGivenLt = {GivenLt = now}
			      val autoIndex = nextval editableTreeInstanceSequence
			      val key_widget = _
			      val widgets = _
			      val tab = editableTreeInstanceTable
			      val givenLtOrd = @@Record.ord [givenLt] _ _
			      val labels = { Geolocation = "Geolocation" }
			      val permission = return {Add = True, TopAdd = True, Delete = True, Modify = True} (* {Add = True, TopAdd = True, Delete = False, Modify = False} *)
			      val givenEqEq = @@Record.eq [givenEq] _ _
			      val amGivenEq = return None (* NO EXPECTED INPUT *)
			      val onAdd = fn _ => return ()
			      val onDelete = fn _ => return ()
			      val onModify = fn _ => return ()
			  end )

fun ensureHost (host : string) : transaction unit =
    h <- getHeader (blessRequestHeader "Host");
    case h of
	None => error (<xml> Different Request Header Host </xml>)
      | Some h => if h = host then return ()
		  else error (<xml> Different Request Header Host </xml>)

cookie lastVisitModeCookie : bool
cookie lastVisitCookie : list (string * time)
			 
val setLastVisitMode =
	setCookie lastVisitModeCookie {Value = True, Expires = None, Secure = False}

val clearLastVisitMode =
	setCookie lastVisitModeCookie {Value = False, Expires = None, Secure = False}
	
fun o1 root publicId : transaction page =
    ensureHost netHost;    
    lastVisitModeCookieVal <- Monad.mp (fn x => case x of
						None => True
					      | Some v => v) (getCookie lastVisitModeCookie);
    lastVisitCookieVal <- Monad.mp (fn x => case x of
						None => (publicId, minTime) :: Nil
					      | Some v => v) (getCookie lastVisitCookie);
    nowVal <- now;
    setCookie lastVisitCookie {Value = (publicId, nowVal) :: List.filter (fn (x, _) => x <> publicId) lastVisitCookieVal, Expires = None, Secure = False};
    givenLt <- return (if lastVisitModeCookieVal then (case (List.assoc publicId lastVisitCookieVal) of None => minTime | Some v => v) else minTime);

    Ui.simple ((case root of None => "/None/" | Some rootv => "/Some/" ^ (show rootv) ^ "/") ^ publicId)
	      (Ui.seq (EditableTreeInstance.ui (root, {GivenEq = publicId}, {GivenLt = givenLt}, {GivenLt = Basis.addSeconds nowVal (60)}),
		       Ui.const (<xml><hr/><div>Clear lastVisit cookie, show all <active code={cS <- source False; set cS (not lastVisitModeCookieVal);
											   return (<xml><ccheckbox source={cS} onclick={fn _ => cSVal <- get cS;
																	   if cSVal then rpc (setLastVisitMode); alert "lastVisitMode now"
																	   else rpc (clearLastVisitMode); redirect (url (o1 root publicId)) } /></xml>) }/> </div></xml>)))


val data : string -> int -> int -> int -> int -> int -> int -> transaction page = Chat.data		   
val edit : string -> transaction page = Chat.edit		   
val redir : string -> transaction page = Chat.redir		   
val listing : unit -> transaction page = Chat.listing		   
val main : string -> transaction page = Chat.main
