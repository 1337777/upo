functor Make (M : sig
		  type subValue
		  type subWidget
		  val widget_subValue_subWidget : Widget.t subValue subWidget
	      end) : sig

    type widget
    val widget_list_widget : Widget.t (list M.subValue) widget
end
