type optionBoxWidget = fn subWidget => source bool * source (option subWidget)
		    
val optionBox = fn [subValue] [subWidget] (widget_subWidget : Widget.t subValue subWidget) =>
		   @@Widget.make [option subValue] [optionBoxWidget subWidget]
		     { Create = trueS <- source True;
		       subWidget <- @Widget.create widget_subWidget;
		       subWidgetOS <- source (Some subWidget);
		       return (trueS, subWidgetOS),
		       Initialize = fn subValueO => case subValueO of
							None => falseS <- source False;
							subWidgetOS <- source None;
							return (falseS, subWidgetOS)
						      | Some subValue => trueS <- source True;
							subWidget <- @Widget.initialize widget_subWidget subValue;
							subWidgetOS <- source (Some subWidget);
							return (trueS, subWidgetOS),
		       AsWidget =
		    fn widget => ( <xml><ccheckbox
				      source={widget.1}
						 onchange={ widgetMode <- get widget.1;
							    case widgetMode of
								True => subWidget <- @Widget.create widget_subWidget;
								set widget.1 True; set widget.2 (Some subWidget)
							      | False => set widget.1 False; set widget.2 None }/>
						   <dyn
		    signal={ widgetMode <- signal widget.1;
			     widgetSubWidgetO <- signal widget.2;
			     case widgetMode of
				 False => return ( <xml/> ) (* *)
				 | True => ( case widgetSubWidgetO of
				 None => return ( <xml/> ) (* temp transition error (<xml>BAD STATE</xml>) *)
				 | Some widgetSubWidget => return (<xml>{@Widget.asWidget widget_subWidget widgetSubWidget}</xml>) ) } /></xml> ),
			   Value = fn widget => widgetMode <- signal widget.1;
				  case widgetMode of
				      False => return None
				    | True => widgetSubWidgetO <- signal widget.2;
				      case widgetSubWidgetO of
					  None => return None (* tmp transition error (<xml>BAD STATE</xml>)*)
					| Some widgetSubWidget =>
					  subValue <- @Widget.value widget_subWidget widgetSubWidget;
					  return (Some subValue),
		       AsValue = fn subValueO => case subValueO of
						     None => (<xml>[NONE]</xml>)
						   | Some subValue => (@Widget.asValue widget_subWidget subValue) }

		     

val test () : transaction page =
    let val optionBoxString = (@@optionBox [string] [_] _) in 
    optionState <- @Widget.create optionBoxString ;
    return (<xml><body> <h1>OPTIONBOXSTRING</h1>
      <hr/>
      {@Widget.asWidget optionBoxString optionState}
      <hr/>
      <p>ASVALUE:</p>
      <hr/>
      <dyn signal={ tu <- @Widget.value optionBoxString optionState;
		    return (Widget.asValue tu) } />
      <hr/>
      <hr/>
    </body></xml>)
    end
