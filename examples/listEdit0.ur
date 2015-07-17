datatype wlist subWidget =
Nil
| Cons of { Data : subWidget,
	    Tail : source (wlist subWidget) }
type widget subWidget =
     { Edit : source subWidget,
       Head : source (wlist subWidget),
       TailP : source (source (wlist subWidget)) }

fun create [subValue] [subWidget] (widget_subValue_subWidget : Widget.t subValue subWidget)  =
    edit <- @Widget.create widget_subValue_subWidget;
    edit <- source edit;
    head <- source (@@Nil [subWidget]);
    tailP <- source head;
    return { Edit = edit, Head = head, TailP = tailP }

fun initialize [subValue] [subWidget] (widget_subValue_subWidget : Widget.t subValue subWidget) (ls : list subValue) =
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

fun asWidget [subValue] [subWidget] (widget_subValue_subWidget : Widget.t subValue subWidget) (widget : widget subWidget) : xbody =
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
	  <button value="Add" onclick={ fn _ => add (); clearEdit () } />
	    <button value="Reset" onclick={ fn _ => reset () } /><br/><br/>
	      <dyn signal={ asWidgetS widget.Head }/></xml> )
    end
    
fun value [subValue] [subWidget] (widget_subValue_subWidget : Widget.t subValue subWidget) (widget : widget subWidget) = 
    let
	fun valueS (wlss : source (wlist subWidget)) : signal (list subValue) =
	    wls <- signal wlss;
	    valueL wls
	    
	and valueL (wls : wlist subWidget) : signal (list subValue) =
	    case wls of
		Nil => return List.Nil
	      | Cons {Data = subWidget, Tail = wlss} =>
		subValue <- @Widget.value widget_subValue_subWidget subWidget;
		ls <- valueS wlss;
		return (List.Cons (subValue, ls))
    in
	valueS widget.Head
    end

fun asValue [subValue] [subWidget] (widget_subValue_subWidget : Widget.t subValue subWidget) (ls : list subValue ) : xbody =
    (<xml><ul>{ List.mapX (fn subValue => <xml><li>{ @Widget.asValue widget_subValue_subWidget subValue }</li></xml>) ls }</ul></xml>)

fun widget_list_widget [subValue] [subWidget] (widget_subValue_subWidget : Widget.t subValue subWidget) : Widget.t (list subValue) (widget subWidget) =
    Widget.make
	{ Create = @create (widget_subValue_subWidget),
	  Initialize = @initialize (widget_subValue_subWidget),
	  AsWidget = @asWidget (widget_subValue_subWidget),
	  Value = @value (widget_subValue_subWidget),
	  AsValue = @asValue (widget_subValue_subWidget) }
    
