function generateSecret() {
    try {
	var curve = "secp256r1";
	var ec = new KJUR.crypto.ECDSA({"curve": curve});
	var keypair = ec.generateKeyPairHex();
	return {_Secret: keypair.ecprvhex, _PublicId: keypair.ecpubhex};
    }
    catch (err1) {
	return {_Secret: "", _PublicId: ""};
    }
}

function author(prvkey, msg1) {
    try {
	var curve = "secp256r1";
	var sigalg = "SHA256withECDSA";

	var sig = new KJUR.crypto.Signature({"alg": sigalg, "prov": "cryptojs/jsrsa"});
	sig.initSign({'ecprvhex': prvkey, 'eccurvename': curve});
	sig.updateString(msg1);
	var sigValueHex = sig.sign();
	return sigValueHex;
    }
    catch (err1) {
	return null;
    }
}

function samePublicIdAuthor(pubkey, msg1, sigval) {
    try {
	var curve = "secp256r1";
	var sigalg = "SHA256withECDSA";

	var sig = new KJUR.crypto.Signature({"alg": sigalg, "prov": "cryptojs/jsrsa"});
	sig.initVerifyByPublicKey({'ecpubhex': pubkey, 'eccurvename': curve});
	sig.updateString(msg1);
	var result = sig.verify(sigval);
	if (result) {
	    return true;
	} else {
	    return false;
	}
    }
    catch (err1) {
	return false;
    }
}

function toLocalStorage(secretPublicId) {
    try {
	if(typeof(Storage) !== "undefined") {
	    localStorage.setItem("secret", secretPublicId._Secret);
	    localStorage.setItem("publicid", secretPublicId._PublicId);
	}
	return {};
    }
    catch (err1) {
	return {};
    }
}

function fromLocalStorage() {
    try {
	if(typeof(Storage) !== "undefined") {
	    return {_Secret : localStorage.getItem("secret"), _PublicId : localStorage.getItem("publicid")};
	} else {
	    return {_Secret : "", _PublicId : ""};
	}
    }
    catch (err1) {
	return {_Secret : "", _PublicId : ""};
    }
}
