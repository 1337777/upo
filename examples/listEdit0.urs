
con widget :: Type -> Type
val widget_list_widget : subValue ::: Type ->
			 subWidget ::: Type ->
			 Widget.t subValue subWidget ->
			 Widget.t (list subValue) (widget subWidget)
			 
