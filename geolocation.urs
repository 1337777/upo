type geolocation = {GeoLat : float, GeoLon : float}

val geolocation : transaction (option geolocation)
val initGeolocation : transaction unit
