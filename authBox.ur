type authBoxWidget = {Auth : source string, ModeSwitch : source bool, Secret : source string, Timeurn : source (option (signal string)), PublicId : source (option (signal string)), AltPublicId : source (option (signal (option string))), Paste : source (option (source string))}

val authBox = @@Widget.make [option string] [authBoxWidget]
		{ Create = Monad.exec {Auth = source "", ModeSwitch = source True, Secret = source "", Timeurn = source None, PublicId = source None, AltPublicId = source None, Paste = source None}, 
		  Initialize = fn sO => Monad.exec {Auth = source (case sO of None => "" | Some s => s), ModeSwitch = source False, Secret = source "", Timeurn = source None, PublicId = source None, AltPublicId = source None, Paste = source None},
		  AsWidget = fn r => ( <xml><dyn signal={ rModeSwitch <- signal r.ModeSwitch;
							  return ( <xml>{ case rModeSwitch of
									      False => ( <xml><ctextbox source={r.Auth}/></xml> )
									    | True => ( <xml><ctextbox source={r.Secret}/> <button onclick={ fn _ => idOS <- get r.Paste; case idOS of None => return () | Some idS => s <- get idS; set r.Secret s }>Paste</button>,</xml> ) } SecretMode: <ccheckbox source={r.ModeSwitch}/></xml> ) } /></xml> ),
		  Value = fn r => timeurnSO <- signal r.Timeurn;
			     case timeurnSO of
				 None => return None (* error ( <xml> pointer to some Timeurn required </xml> ) *)
			       | Some timeurnS =>
				 altPublicIdOSO <- signal r.AltPublicId;
				 (case altPublicIdOSO of
				      None => return None (* error ( <xml>pointer to AltPublicId is required, nomatterwhat</xml> ) *)
				    | Some altPublicIdOS =>
				      altPublicIdO <- altPublicIdOS;
				      finalPublicIdO <- (case altPublicIdO of
							     None =>
							     publicIdSO <- signal r.PublicId;
							     (case publicIdSO of
								  None => return None (* error ( <xml>at least PublicId , AltPublicId shall content string</xml> ) *)
								| Some publicIdS => Monad.mp Some publicIdS)
							   | _ => return altPublicIdO);
				      (case finalPublicIdO of
					   None => return None
					 | Some finalPublicId =>
					   authorO <- (rModeSwitch <- signal r.ModeSwitch;
						       case rModeSwitch of
							   False => Monad.mp Some (signal r.Auth)
							 | True => Monad.liftM2 Ecdsa.author (signal r.Secret) timeurnS);
					   (case authorO of
						None => return None
					      | Some author =>
						timeurn <- timeurnS;
						(case Ecdsa.samePublicIdAuthor finalPublicId timeurn author of
						     False => return None
						   | True => return (Some author) )))),
		  AsValue = fn sO => case sO of None => <xml><p>KO</p></xml>
					      | Some s => <xml><p title={s}>OK</p></xml> }

	      
