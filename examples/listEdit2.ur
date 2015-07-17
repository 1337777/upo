
structure ListEditString = ListEdit.Make (struct
					      type subValue = string
					      type subWidget = _
					      val widget_subValue_subWidget = _
					  end)

val main () : transaction page =
    let
	val widget_listString_widget = (@@ListEdit0.widget_list_widget [string] [_] _)
    in
	widget0 <- let @Widget.initialize widget_listString_widget ls
		       where val ls = "AAAA000" :: "BBBB000" :: "CCCC0000" :: Nil
		   end;
	widget <- let @Widget.initialize ListEditString.widget_list_widget ls
		      where val ls = "AAAA" :: "BBBB" :: "CCCC" :: Nil
		  end;

	Ui.tabbed
	    "ListEdit"
	{ Tab1 = (Some "ListEdit0 Type Class",
		  Ui.const (<xml><h3>As widget</h3>
		    <dyn signal={ return (@Widget.asWidget widget_listString_widget widget0) } />
		    <h3>As value</h3>
		    <dyn signal={ value <- @@Widget.value [list string] [_] _ widget0;
				  return (Widget.asValue value) } /></xml>) ),
	  Tab2 = (Some "ListEdit Funtor",
		  Ui.const (<xml><h3>As widget</h3>
		    <dyn signal={ return (@Widget.asWidget ListEditString.widget_list_widget widget) } />
		    <h3>As value</h3>
		    <dyn signal={ value <- @Widget.value ListEditString.widget_list_widget widget;
				  return (Widget.asValue value) } /></xml>) ) }
    end
