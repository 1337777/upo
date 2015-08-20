var orgPrefix = "http://127.0.0.1:8082/";
var urlPrefix = orgPrefix;

function setIt(id, html) {
    setInnerHTML(document.getElementById(id), html);
}

function getIt(id) {
    var elementId = document.getElementById(id);
    if (elementId == null) {
	return null;
    } else {
	return elementId.innerHtml;
    }
}

function getValue(id) {
    var elementId = document.getElementById(id);
    if (elementId == null) {
	return null;
    } else {
	var s = new String(elementId.value);
	return s.toString();
    }
}

function getUrlPrefix() {
    return urlPrefix;
}

function setUrlPrefix(s) {
    try {
	var oldUrlPrefix = urlPrefix;
	urlPrefix = s;
	var x = document.getElementsByClassName("EditableTree_aPrefix");
	for (i = 0; i < x.length; i++) {
	    x[i].href = x[i].href.replace(oldUrlPrefix, urlPrefix);
	    x[i].innerHTML = x[i].innerHTML.replace(oldUrlPrefix, urlPrefix);
	} 
	return {};
    }
    catch (err1) {
	return {};
    }
}

function windowOpen(loc) {
    window.open(loc);
}

function rand() {
    var x = Math.floor(Math.random() * 1000000000 /* Number.MAX_VALUE */ );
    return x;
}

function transformxml(xslttxt, len)
{
    try {
	// Mozilla
	var doc = new DOMParser();
	var xml = doc.parseFromString('<?xml version="1.0" encoding="UTF-8"?><html></html>',"text/xml");
	var xsltPrs = new XSLTProcessor();
	var mydiv = document.getElementsByClassName("Chat_xsltOutput")[0];

	var xsl = doc.parseFromString('<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"><xsl:template match="/"><html></html></xsl:template></xsl:stylesheet>',"text/xml");
	xsltPrs.importStylesheet(xsl);
	xml = xsltPrs.transformToDocument(xml);
	
	var ij = 0;
	for (ij = 0; ij < len; ij++) {
	    xsl = doc.parseFromString(xslttxt["_1"],"text/xml");
	    xsltPrs.importStylesheet(xsl);
	    xml = xsltPrs.transformToDocument(xml);
	    xslttxt = xslttxt["_2"];
	}
	

	xsl = doc.parseFromString(
	    '<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"><xsl:template match="/"><xsl:copy-of select="."/></xsl:template></xsl:stylesheet>' ,
	    "text/xml");
	xsltPrs.importStylesheet(xsl);
	xml = xsltPrs.transformToFragment(xml, document);
	// if (xml.firstChild.nodeName == "PARSERERROR") { return false; }

	// if (mydiv.firstChild != null) { mydiv.removeChild(mydiv.firstChild); }
	mydiv.innerHTML = "";
	mydiv.appendChild(xml);
	return true
    }
    catch (err1) {
	return false
    }
}
