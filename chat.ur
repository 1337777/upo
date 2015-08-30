val netHost = "localhost:8081"
val orgHost = "127.0.0.1:8082"
val netPrefix = "http://" ^ netHost ^ "/o1/"
val orgPrefix = "http://" ^ orgHost ^ "/"

style xsltOutput

structure Room = Broadcast.Make(struct
                                    type t = string
                                end)

cookie touchedCookie : (* list publickey * *) time

fun data (titl : string) (dt1 : int) (dt2 : int) (dt3 : int) (dt4 : int) (dt5 : int) (dt6 : int)
    : transaction page =
    nowVal <- now;
    dtTime <- return (fromDatetime dt1 (dt2 - 1) dt3 dt4 dt5 dt6);
    dtTime <- return (if dt1 = 0 || dtTime = minTime then nowVal else dtTime);
    (* debug (show dt1 ^ "~" ^ show dt2 ^ "~" ^ show dt3 ^ "~" ^ show dt4 ^ "~" ^ show dt5 ^ "~" ^ show dt6 ^ "~" ^ show dtTime); *)
    (id, _) <- Room.linked titl;
    (_, alarmsInitial) <- Room.subscribe id dtTime;
    alarmsInitial <- return (List.mp (fn x => (True, x.Text)) alarmsInitial);
    alarmsInitial <- return (List.mp (fn x => case x.2 of None => "" | Some x2 => x2) (List.filter (fn x => x.1 = True) alarmsInitial));

    let
        fun onload () =
	    (* debug: alert (show dt1 ^ "~" ^ show dt2 ^ "~" ^ show dt3 ^ "~" ^ show dt4 ^ "~" ^ show dt5 ^ "~" ^ show dt6 ^ "~" ^ show dtTime); *)
	    b <- SetInner.transformxml alarmsInitial (List.length alarmsInitial);
	    if Top.not b then
		error (<xml> Bad xslt list on load</xml>)
	    else return ()
    in
	 return (<xml><body>
	   <div class={xsltOutput}></div>
	   <div><active code={onload (); return (<xml/>)}/> (* script to erase this div active node *)
	   </div>
	 </body></xml>)
    end
    
fun edit (titl : string) =
    nowVal <- now;
    (id, pid) <- Room.linked titl;
    pid <- return (case pid of None => "[NOT LINKED]" (* error *) | Some x => x);
    (ch : channel (serialized (bool * (option string) * time)),
     alarmsInitial) <- Room.subscribe id nowVal;
    (alarmsInitial : list (bool * (option string) *time)) <- return (List.mp (fn x => (True, x.Text, x.Timestamp)) alarmsInitial);
    newLine <- source "";
    db <- source False;
    buf <- source alarmsInitial;

    let
        fun onload () =
            let
                fun listener () =
                    msg <- recv ch;
		    msg <- return (deserialize msg);
		    ls <- get buf;
		    ls <- return (List.append ls (msg :: []));
		    set buf ls;
		    ls <- return (List.mp (fn x => case x.2 of None => "" | Some x2 => x2) (List.filter (fn x => x.1 = True) ls));
		    b <- SetInner.transformxml ls (List.length ls);
		    if Top.not b then
			error (<xml> Bad xslt list after receive</xml>)
		    else listener ()
            in
		ls <- get buf;
		ls <- return (List.mp (fn x => case x.2 of None => "" | Some x2 => x2) (List.filter (fn x => x.1 = True) ls));
		b <- SetInner.transformxml ls (List.length ls);
		if Top.not b then
		    error (<xml> Bad xslt list on load</xml>)
		else listener ()
            end

        fun doSpeak () =
            line <- get newLine;
	    dbVal <- get db;
	    ls <- get buf;

	    if dbVal then
		spawn (
		sleep 1000;
		lsCons <- return (List.mp (fn x => case x.2 of None => "" | Some x2 => x2) (List.filter (fn x => x.1 = True) (List.append ls ((dbVal, Some line, minTime(*dummy*)) :: []))));
		b <- SetInner.transformxml lsCons (List.length lsCons);
		if b then
		    rpc (Room.send id (dbVal, Some line));
		    set newLine ""
		else
		    ls <- return (List.mp (fn x => case x.2 of None => "" | Some x2 => x2) (List.filter (fn x => x.1 = True) ls));
		    _ <- SetInner.transformxml ls (List.length ls);
		    alert "Bad xslt at doSpeak"
		)
	    else
		rpc (Room.send id (dbVal, Some line));
		set newLine ""

    in
        return ( <xml><body onload={onload ()}>
	  <div style="float: left; max-width: 400px">
            <button value="Send:" onclick={fn _ => doSpeak ()}/> Xslt? <ccheckbox source={db}/><br/>
	      <ctextarea source={newLine} cols={40} rows={12} style="width: 100%"/>
	      <div style="overflow: scroll; max-height: 600px">
		<dyn signal={ls <- signal buf;
			     return (List.mapX (fn x => <xml><li>{[show x.3]} @ {[case x.2 of None => "" | Some x2 => x2]}</li></xml>) ( List.take 100 (List.rev ls) ) )
			    }/></div></div>
		
	      <div class={xsltOutput} style="float: left"></div>
        </body></xml> )
    end

fun ensureHost (host : string) : transaction unit =
    h <- getHeader (blessRequestHeader "Host");
    case h of
	None => error (<xml> Different Request Header Host </xml>)
      | Some h => if h = host then return ()
		  else error (<xml> Different Request Header Host </xml>)

fun redir (titl : string) : transaction page =
    ensureHost orgHost;
    (id, pid) <- Room.linked titl;
    pid <- return (case pid of None => "" (* error *) | Some x => x);
    redirect (bless (netPrefix ^ pid))

fun listing () : transaction page =
    ensureHost orgHost;
    lsS <- source (<xml></xml>);
    
    let
	val myGet : transaction unit =
	    ls <- rpc (Room.listed);
	    ls <- List.mapXM (fn r => count <- rpc (Room.subscribers r.Id);
				 return (<xml><tr>
				   <td>{[r.Timestamp]}</td>			       
				   <td><a link={edit r.Title}>{[r.Title]}</a></td>
				   <td><a href={bless (netPrefix ^ (case r.PublicId of None => "" | Some x => x))}>{[r.PublicId]}</a></td>
				   <td>{[count]}</td>
			     </tr></xml>)) ls;
	    ls <- return (<xml><table>
              <tr> <th>Created</th> <th>Title</th> <th>PublicId</th> <th>#Subscribers</th> </tr>
	      {ls} </table></xml>);
	    set lsS ls
    in
	return (<xml><body onload={myGet}>
	  <h1>Current Channels</h1>
	  <dyn signal={signal lsS}/>
	  
	  <h1>New Channel</h1>
	  <active code={ titlS <- source "";
			 pidS <- source "";
			 return (<xml>Title: <ctextbox source={titlS}/><br/>
			   Link to: <ctextbox source={pidS}/><br/>
			   <button onclick={fn _ => titl <- get titlS;
					       pid <- get pidS;
					       _ <- rpc (Room.create titl pid);
					       set titlS "";
					       set pidS "";
					       myGet}>
			     Create</button></xml>) }/>
			 </body></xml>)
    end
    
    
fun main (titl : string) : transaction page =
    ensureHost orgHost;
    if titl <> "" then redir titl else
    listing ()
