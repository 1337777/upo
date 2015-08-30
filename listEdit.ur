functor Make (M : sig
		  type subValue
		  type subWidget
		  val widget_subValue_subWidget : Widget.t subValue subWidget
	      end) = struct
    open M
	 
    datatype wlist  =
	     Nil
	   | Cons of { Data : subWidget,
		       Tail : source wlist }
    type widget =
	 { Edit : source subWidget,
	   Head : source wlist,
	   TailP : source (source wlist) }

    val create =
	edit <- @Widget.create widget_subValue_subWidget;
	edit <- source edit;
	head <- source Nil;
	tailP <- source head;
	return { Edit = edit, Head = head, TailP = tailP }

    fun initialize (ls : list subValue) =
	let
	    fun initialize0 ls =
		case ls of
		    List.Nil =>
		    head <- source Nil;
		    tailp <- source head;
		    return (head, tailp)
		  | List.Cons (x, ls) =>
		    subWidget <- @Widget.initialize widget_subValue_subWidget x;
		    (head, tailp) <- initialize0 ls;
		    newHead <- source (Cons { Data = subWidget, Tail = head });
		    return (newHead, tailp)
	in
	    edit <- @Widget.create widget_subValue_subWidget;
	    edit <- source edit;
	    (head, tailp) <- initialize0 ls;
	    return { Edit = edit, Head = head, TailP = tailp }
	end

    fun asWidget (widget : widget) : xbody =
	let
	    fun asWidgetS wlss =
		wls <- signal wlss;
		asWidgetL wls

	    and asWidgetL wls =
		case wls of
		    Nil => return ( <xml></xml> )
		  | Cons { Data = subWidget, Tail = wlss } => 
		    return ( <xml> { @Widget.asWidget widget_subValue_subWidget subWidget } <br/>
		      <dyn signal={asWidgetS wlss} /> </xml> )

	    fun add () =
		edit <- get widget.Edit;
		subValue <- current (@Widget.value widget_subValue_subWidget edit);
		subWidget <- @Widget.initialize widget_subValue_subWidget subValue;
		tail' <- source Nil;
		let
		    val cons = Cons {Data = subWidget, Tail = tail'}
		in
		    tail <- get widget.TailP;
		    set tail cons;
		    set widget.TailP tail';
		    
		    head0 <- get widget.Head;
		    case head0 of
			Nil => set widget.Head cons
		      | _ => return ()
		end

	    fun clearEdit () =
		edit <- @Widget.create widget_subValue_subWidget;
		set widget.Edit edit
		
	    fun reset () =
		set widget.Head Nil;
		set widget.TailP widget.Head;
		clearEdit ()
		
	in
	    ( <xml><dyn signal={ edit <- signal widget.Edit; return (@Widget.asWidget widget_subValue_subWidget edit) } />
	      <button value="➕"(*"Add"*) onclick={ fn _ => add (); clearEdit () } />
		<button value="✖"(*"Reset"*) onclick={ fn _ => reset () } /><br/><br/>
		  <dyn signal={ asWidgetS widget.Head }/></xml> )
	end
	
    fun value (widget : widget) = 
	let
	    fun valueS (wlss : source wlist) : signal (list subValue) =
		wls <- signal wlss;
		valueL wls
		
	    and valueL (wls : wlist) : signal (list subValue) =
		case wls of
		    Nil => return List.Nil
		  | Cons {Data = subWidget, Tail = wlss} =>
		    subValue <- @Widget.value widget_subValue_subWidget subWidget;
		    ls <- valueS wlss;
		    return (List.Cons (subValue, ls))
	in
	    valueS widget.Head
	end

    fun asValue (ls : list subValue ) : xbody =
	(<xml><ul>{ List.mapX (fn subValue => <xml><li>{ @Widget.asValue widget_subValue_subWidget subValue }</li></xml>) ls }</ul></xml>)

    val widget_list_widget : Widget.t (list subValue) widget =
	Widget.make
	    { Create = create,
	      Initialize = initialize,
	      AsWidget = asWidget,
	      Value = value,
	      AsValue = asValue }

end
