type optionBoxWidget = fn subWidget => source bool * source (option subWidget)
		    
val optionBox : subValue ::: Type -> subWidget ::: Type -> Widget.t subValue subWidget
		-> Widget.t (option subValue) (optionBoxWidget subWidget)
