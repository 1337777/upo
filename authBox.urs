type authBoxWidget = {Auth : source string, ModeSwitch : source bool, Secret : source string, Timeurn : source (option (signal string)), PublicId : source (option (signal string)), AltPublicId : source (option (signal (option string))), Paste : source (option (source string))}

val authBox : Widget.t (option string) (authBoxWidget)
