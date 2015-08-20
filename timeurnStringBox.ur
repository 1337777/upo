style aPrefix

fun filterSpace s =
    let
        fun filterSpace0 i acc =
            if i < 0 then
                acc
            else
		let
		    val ch = Basis.strsub s i
		in
                    filterSpace0 (i - 1)
				 ( (if Basis.isspace ch then "" else Basis.str1 ch) ^ acc )
		end
    in
        filterSpace0 (Basis.strlen s - 1) ""
    end

fun filterPrefixes s =
    let 
	fun filterPrefix r =
	    case String.ssplit r of
		None => r.Haystack
	      | Some (prefix, suffix) => filterPrefix {Haystack = suffix, Needle = r.Needle}
    in
	filterPrefix {Haystack = filterPrefix {Haystack = String.substring s {Start = 0, Len = 1024}, Needle = "http://"}, Needle = "data:"}
    end
		  
val timeurnBox = @@Widget.make [time * string] [source string * source string]
		   { Create = Monad.exec (nowVal <- now; source (Basis.timef "%Y-%m-%dT%H:%M:%S" nowVal),
					  source ""),
		     Initialize = fn (t, s) => Monad.exec (source (Basis.timef "%Y-%m-%dT%H:%M:%S" t), source s),
		     AsWidget = fn (tS, sS) => ( <xml><cdatetime_local source={tS}/>
		       <ctextbox source={sS}/></xml> ),
		     Value = fn (tS, sS) => t <- signal tS;
				s <- signal sS;
				return (Option.get minTime (read t),
					filterPrefixes (filterSpace s)),
		     AsValue = fn (t, s) => if t = minTime
					    then <xml><b>INVALID</b></xml>
					    else <xml>{txt t} (*blink-alternate urls here*)
					      <active code={ urlPrefix <- SetInner.getUrlPrefix();
					        case Basis.checkUrl (urlPrefix ^ s) of
						    None => return <xml><b>[BLOCKED URL]</b></xml>
						  | Some url => return <xml><a href={url} class={aPrefix} target="_blank"><tt>{[url]}</tt></a></xml> }/></xml> }
		 
structure ListEditTimeUrn = ListEdit.Make (struct
					       type subValue = time * string
					       type subWidget = _
					       val widget_subValue_subWidget = timeurnBox
					   end)
			    
val json_time = @@Meta.Json.mkJson [time] { ToJson = fn t => @Meta.Json.toJson Meta.Json.json_string (Basis.timef "%Y-%m-%dT%H:%M:%S" t),
					    FromJson = fn sJson => let val s = @Meta.Json.fromJson' Meta.Json.json_string sJson in (readError s.1, s.2) end }

val json_timestring = @Meta.Json.json_record _ (json_time, Meta.Json.json_string) ("1","2")

type timeurnStringBox = ListEditTimeUrn.widget
			
val timeurnStringBox = @@Widget.make [string] [timeurnStringBox]
			 { Create = @Widget.create ListEditTimeUrn.widget_list_widget,
			   Initialize = fn s => @Widget.initialize ListEditTimeUrn.widget_list_widget (Meta.Json.fromJson s),
			   AsWidget = @Widget.asWidget ListEditTimeUrn.widget_list_widget,
			   Value = fn widgetState => ls <- @Widget.value ListEditTimeUrn.widget_list_widget widgetState;
				      return (Meta.Json.toJson ls),
			   AsValue = fn s => (<xml>{@Widget.asValue ListEditTimeUrn.widget_list_widget (Meta.Json.fromJson s)}
			     <active code={alarmSwitch <- source False;
					   forceNow <- source False;
					   let
					       fun alarmLoop ls runTime =
						   alarmSwitchVal <- current (signal alarmSwitch);
						   if alarmSwitchVal then
						       case ls of
										 | Nil => return ()
										 | hd :: tl =>
										   if hd.1 <= runTime then
										       urlPrefix <- SetInner.getUrlPrefix();
										       SetInner.windowOpen (urlPrefix ^ hd.2);
										       sleep 1000;
										       alarmLoop tl (runTime + 1)
										   else
										       sleep 1000;
										       alarmLoop ls (runTime + 1)
						   else return ()
					   in
					       return (<xml>Run <ccheckbox source={alarmSwitch}/>
						             <dyn signal={ alarmSwitchVal <- (signal alarmSwitch);
									   forceNowVal <- (signal forceNow);
									   return (<xml><active code={	   
										   nowVal <- now;
										   let
										       val ls : list (time * string) = (Meta.Json.fromJson s)
										       val ls = List.sort (fn x y => x.1 > y.1) ls
										       val ls = case ls of
												    Nil => Nil (* /!\ ls hehe *)
												  | hd :: tl => if hd.1 < nowVal || forceNowVal then
														    (0, hd.2) :: (List.mp (fn x => (let val x1diff = diffInSeconds hd.1 x.1
																		    in if x1diff < 0 then 0 else x1diff end,
																		    x.2)) tl)
														else List.mp (fn x => (let val x1diff = diffInSeconds nowVal x.1
																       in if x1diff < 0 then 0 else x1diff end,
																       x.2)) ls
										   in
										       if alarmSwitchVal then
											   spawn (alarmLoop ls 0);
											   return (<xml/>) (* *)
										       else return (<xml>, Force from now <ccheckbox source={forceNow}/></xml>) (*  *)
										   end}/></xml>) }/></xml>)
					   end}/></xml>) }

