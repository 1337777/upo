
type geolocationBox = source (option Geolocation.geolocation)

val geolocationBox = @@Widget.make [option Geolocation.geolocation] [geolocationBox]
		       { Create = source None,
			 Initialize = source,
			 AsWidget = fn glS => ( <xml><active code={
						let
						    fun loop n =
							if n > 10 then
							    return ()
							else
							    ( gl <- Geolocation.geolocation;
							      case gl of
								  None => sleep 1000; loop (n+1)
								| Some gl => set glS (Some gl) )
						in
						    Geolocation.initGeolocation;
						    spawn (loop 0);
						    return ( <xml><dyn signal={
							     gl <- signal glS;
							     case gl of
								 None => return ( <xml/> )  (* *)
								 | Some gl => return ( <xml>{[gl.GeoLat]} , {[gl.GeoLon]}</xml> ) }/></xml> )
						end } /></xml> ),
			 Value = signal,
			 AsValue = fn gl => case gl of
						None => ( <xml></xml> )
					      | Some gl => ( <xml>{[gl.GeoLat]} , {[gl.GeoLon]}</xml> ) }

		     
val geolocationBoxDb = @@Widget.make [serialized(option Geolocation.geolocation)] [geolocationBox]
		       { Create = @Widget.create geolocationBox,
			 Initialize = deserialize >>> @Widget.initialize geolocationBox,
			 AsWidget = @Widget.asWidget geolocationBox,
			 Value = @Widget.value geolocationBox >>> (Monad.mp serialize),
			 AsValue = deserialize >>> @Widget.asValue geolocationBox  }

val json_geolocation0 : Meta.Json.json Geolocation.geolocation = Meta.Json.json_record {GeoLat = "geoLat",  GeoLon = "geoLon"}
val json_geolocation : Meta.Json.json (option Geolocation.geolocation) = @Meta.Json.json_option json_geolocation0
								
val serialize = @Meta.Json.toJson json_geolocation
val deserialize = @Meta.Json.fromJson json_geolocation

val geolocationBoxJson = @@Widget.make [string] [geolocationBox]
		       { Create = @Widget.create geolocationBox,
			 Initialize = deserialize >>> @Widget.initialize geolocationBox,
			 AsWidget = @Widget.asWidget geolocationBox,
			 Value = @Widget.value geolocationBox >>> (Monad.mp serialize),
			 AsValue = deserialize >>> @Widget.asValue geolocationBox  }

(*
 fun remoteAddr () =
     getenv (blessEnvVar "REMOTE_ADDR") *)
