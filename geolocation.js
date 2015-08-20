var _geolocation = null;

function initGeolocation () {
    if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition (
	    function (position) {
	        _geolocation = { _GeoLat : position.coords.latitude,
			       _GeoLon : position.coords.longitude };
	    } );
    } else { 
        _geolocation = null;
    }
}

function geolocation () {
    return _geolocation;
}


