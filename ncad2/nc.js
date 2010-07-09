var tabLinks = new Array();
var contentDivs = new Array();
var atomArray;

var mouseIsDown = false;
var previousX = 0;
var previousY = 0;

var numAtoms = 20;
var FAST_DRAW_THRESHOLD = 40;

var currentMode;

var orientation;
var boundingBox;
var bondList;

var count = 0;

function log(s) {
    var area = document.getElementById("jslog");
    area.innerHTML += "<br>" + s;
}

function logStringArray(sa) {
    var area = document.getElementById("jslog");
    area.innerHTML += "<br>" + sa.join("<br>");
}

// http://www.w3.org/TR/2000/WD-DOM-Level-1-20000929/level-one-html.html

function getUsernamePassword() {
    var name = document.getElementById("username").value;
    var pw = document.getElementById("password").value;
    alert("User accounts are not yet implemented.");
    log("Name=" + name);
    log("Password=" + pw);
}

function init() {
    log("start init");
    document.getElementById("login").onclick = getUsernamePassword;
    document.getElementById("signup").onclick = getUsernamePassword;
    document.getElementById("propane").onclick = propane;
    document.getElementById("peptide").onclick = cpeptide;
    var tabListItems = document.getElementById('tabs').childNodes;
    for ( var i = 0; i < tabListItems.length; i++ ) {
	if ( tabListItems[i].nodeName == "LI" ) {
	    var tabLink = getFirstChildWithTagName( tabListItems[i], 'A' );
	    var id = getHash( tabLink.getAttribute('href') );
	    tabLinks[id] = tabLink;
	    contentDivs[id] = document.getElementById( id );
	}
    }
    var i = 0;
    for ( var id in tabLinks ) {
	tabLinks[id].onclick = showTab;
	if ( i == 0 ) tabLinks[id].className = 'selected';
	i++;
    }
    var i = 0;
    for ( var id in contentDivs ) {
	if ( i != 0 ) contentDivs[id].className = 'tabContent hide';
	i++;
    }
    var canvas = document.getElementById("canvas");
    var context = canvas.getContext("2d");
    document.onmousedown = mouseDown;
    document.onmousemove = mouseMove;
    document.onmouseup = mouseUp;

    //propane(atomArray);
    cpeptide(atomArray);
}

function makeVector(x,y,z) {
    this.getX = function() { return x; }
    this.getY = function() { return y; }
    this.getZ = function() { return z; }
    this.toString = function() {
	return "[ vector " + x + " " + y + " " + z + " ]";
    };
    this.plus = function(other) {
	return new makeVector(x + other.getX(),
			      y + other.getY(),
			      z + other.getZ());
    }
    this.scale = function(factor) {
	return new makeVector(x * factor,
			      y * factor,
			      z * factor);
    }
    this.distsq = function(other) {
        dx = x - other.getX();
        dy = y - other.getY();
        dz = z - other.getZ();
        return dx * dx + dy * dy + dz * dz;
    }
    this.reorient = function() {
	return orientation.transform(this);
    }
}

function makeAtom(pos,radius,color) {
    // velocity? previous position? force vector?
    this.getPos = function() {
	return pos;
    }
    this.vdwRadius = function() {
        return radius;
    }
    this.reorient = function() {
	var newpos = orientation.transform(pos);
	var r = orientation.P * radius;  // apply perspective
	return new makeAtom(newpos, r, color);
    }
    this.toString = function() {
	return "[ atom " + color + " " + pos.toString() + " ]";
    }
    this.nudge = function(delta) {
	return new makeAtom(pos.plus(delta), radius, color);
    }
    this.draw = function(canvas,context) {
	var K = 300;
	var x = K * pos.getX() + canvas.width/2;
	var y = K * pos.getY() + canvas.height/2;
	var r = K * radius;
	// solid color is much faster than radial gradient
	context.beginPath();
	context.arc(x, y, r, 0, 2 * Math.PI, false);
	context.closePath();
	context.fillStyle = color;
	context.fill();
	context.beginPath();
	context.lineWidth = 1;
	context.strokeStyle = '#000';
	context.arc(x, y, r, 0, 2 * Math.PI*2, true);
	context.stroke();
    }
}

function drawAtoms() {
    var canvas = document.getElementById("canvas");
    var context = canvas.getContext("2d");
    context.fillStyle = "#bdd";
    context.fillRect(0, 0, canvas.width, canvas.height);
    rotArray = new Array();
    for (var i = 0; i < numAtoms; i++) {
	rotArray[i] = atomArray[i].reorient();
    }
    rotArray.sort(function(atom1,atom2) {
	    if (atom1.getPos().getZ() > atom2.getPos().getZ()) return 1;
	    if (atom1.getPos().getZ() < atom2.getPos().getZ()) return -1;
	    return 0;
	});
    for (var i = 0; i < numAtoms; i++) {
	rotArray[i].draw(canvas, context);
    }
}

function determineBonds() {
    var MAXBONDLENGTH = 3 * scalar;
    var MAXLENGTHSQ = MAXBONDLENGTH * MAXBONDLENGTH;
    var SLOP = 1.2;
    // var SLOP = 3.0;
    var buckets = new Array();
    bondList = new Array();
    bucketkey = function(atom) {
        pos = atom.getPos();
        x = pos.getX();
        y = pos.getY();
        z = pos.getZ();
        return [Math.floor(pos.getX() / MAXBONDLENGTH),
                Math.floor(pos.getY() / MAXBONDLENGTH),
                Math.floor(pos.getZ() / MAXBONDLENGTH)];
    }
    // first sort all the atoms into buckets, this is O(N)
    for (var i = 0; i < numAtoms; i++) {
        atom = atomArray[i];
        key = bucketkey(atom);
        if (key in buckets) {
            bucket = buckets[key];
        } else {
            bucket = new Array();
            buckets[key] = bucket;
        }
        bucket[bucket.length] = atom;
    }
    // use the buckets to find all the bonds in O(N) time
    // In the innermost loop where it says MAXBONDLENGTH, it should
    // really be a bond length chosen based on the two atoms, e.g. an
    // H-H bond will be shorter than a C-C bond.
    for (var i = 0; i < numAtoms; i++) {
        var atom = atomArray[i];
        var key = bucketkey(atom);
        var x1 = key[0];
        var y1 = key[1];
        var z1 = key[2];
        var theseBonds = new Array();
        var pos = atom.getPos();
        var r1 = atom.vdwRadius();
        for (var x2 = x1 - 1; x2 < x1 + 2; x2++)
            for (var y2 = y1 - 1; y2 < y1 + 2; y2++)
                for (var z2 = z1 - 1; z2 < z1 + 2; z2++) {
                    var key2 = [x2, y2, z2];
                    for (var j in buckets[key2]) {
                        var otherAtom = atomArray[j];
                        var bondlen = SLOP * (r1 + otherAtom.vdwRadius());
                        if (pos.distsq(otherAtom.getPos()) < bondlen * bondlen) {
                            theseBonds[theseBonds.length] = j;
                        }
                    }
                }
        bondList[i] = theseBonds;
    }
}

function determineBoundingBox() {
    // determine the bounding box
    var xmin = 1.0e20;
    var ymin = 1.0e20;
    var zmin = 1.0e20;
    var xmax = -1.0e20;
    var ymax = -1.0e20;
    var zmax = -1.0e20;
    for (var i = 0; i < numAtoms; i++) {
	pos = atomArray[i].getPos();
        x = pos.getX();
        y = pos.getY();
        z = pos.getZ();
        if (x < xmin) xmin = x;
        if (x > xmax) xmax = x;
        if (y < ymin) ymin = y;
        if (y > ymax) ymax = y;
        if (z < zmin) zmin = z;
        if (z > zmax) zmax = z;
    }
    boundingBox = [new makeVector(xmin, ymin, zmin),
                   new makeVector(xmin, ymin, zmax),
                   new makeVector(xmin, ymax, zmin),
                   new makeVector(xmin, ymax, zmax),
                   new makeVector(xmax, ymin, zmin),
                   new makeVector(xmax, ymin, zmax),
                   new makeVector(xmax, ymax, zmin),
                   new makeVector(xmax, ymax, zmax)];
}

/*
 * When rotating the molecule, we want to draw quickly, so if there
 * are large numbers of atoms, just draw some of them. 
 */
function drawFast() {
    var canvas = document.getElementById("canvas");
    var context = canvas.getContext("2d");
    context.fillStyle = "#bed";
    context.fillRect(0, 0, canvas.width, canvas.height);
    context.strokeStyle = '#000';
    drawLine = function(x1, y1, x2, y2) {
	var K = 300;
	x1 = K * x1 + canvas.width/2;
	y1 = K * y1 + canvas.height/2;
	x2 = K * x2 + canvas.width/2;
	y2 = K * y2 + canvas.height/2;
	context.beginPath();
	context.moveTo(x1, y1);
	context.lineTo(x2, y2);
	context.stroke();
    }
    if (numAtoms < FAST_DRAW_THRESHOLD) {
        rotArray = new Array();
        for (var i = 0; i < numAtoms; i++) {
            rotArray[i] = atomArray[i].reorient();
        }
        for (var i = 0; i < bondList.length; i++) {
            for (var k = 0; k < bondList[i].length; k++) {
                var j = bondList[i][k];
                pos1 = rotArray[i].getPos();
                pos2 = rotArray[j].getPos();
                drawLine(pos1.getX(), pos1.getY(), pos2.getX(), pos2.getY());
            }
        }
    } else {
        var bb000 = boundingBox[0].reorient();
        var bb001 = boundingBox[1].reorient();
        var bb010 = boundingBox[2].reorient();
        var bb011 = boundingBox[3].reorient();
        var bb100 = boundingBox[4].reorient();
        var bb101 = boundingBox[5].reorient();
        var bb110 = boundingBox[6].reorient();
        var bb111 = boundingBox[7].reorient();
        drawLine(bb000.getX(), bb000.getY(), bb001.getX(), bb001.getY());
        drawLine(bb001.getX(), bb001.getY(), bb011.getX(), bb011.getY());
        drawLine(bb011.getX(), bb011.getY(), bb010.getX(), bb010.getY());
        drawLine(bb010.getX(), bb010.getY(), bb000.getX(), bb000.getY());
        drawLine(bb100.getX(), bb100.getY(), bb101.getX(), bb101.getY());
        drawLine(bb101.getX(), bb101.getY(), bb111.getX(), bb111.getY());
        drawLine(bb111.getX(), bb111.getY(), bb110.getX(), bb110.getY());
        drawLine(bb110.getX(), bb110.getY(), bb100.getX(), bb100.getY());
        drawLine(bb100.getX(), bb100.getY(), bb000.getX(), bb000.getY());
        drawLine(bb101.getX(), bb101.getY(), bb001.getX(), bb001.getY());
        drawLine(bb111.getX(), bb111.getY(), bb011.getX(), bb011.getY());
        drawLine(bb110.getX(), bb110.getY(), bb010.getX(), bb010.getY());
    }
}

// http://en.wikipedia.org/wiki/Van_der_Waals_radius

var scalar = 0.2;

function baseAtom(x, y, z, radius, color) {
    atom = new makeAtom(new makeVector(scalar * x,
                                       scalar * y,
                                       scalar * z),
                        4.0 * scalar * radius, color);
    atomArray[atomArray.length] = atom;
}

function carbon(x, y, z) {
    baseAtom(x, y, z, 0.17, "#444");
}

function hydrogen(x, y, z) {
    baseAtom(x, y, z, 0.12, "#fff");
}

function oxygen(x, y, z) {
    baseAtom(x, y, z, 0.152, "#f00");
}

function nitrogen(x, y, z) {
    baseAtom(x, y, z, 0.155, "#00f");
}

function sulfur(x, y, z) {
    baseAtom(x, y, z, 0.18, "#cc0");
}

function propane() {
    scalar = 0.2;

    atomArray = new Array();
    carbon(-0.85748833891984, 0.34794546061805, 0.98214417192606);
    carbon(0.4065315061009, -0.28637730259981, 0.32481170697201);
    carbon( 0.53857043456917, -0.1123325081226, -1.2159381954418);
    hydrogen(1.3229946018417, 0.041931101228472, 0.86981518860896);
    hydrogen(0.53148792074281, -1.3725391912445, 0.54573546004739);
    hydrogen(-0.80732381517125, 0.36113988946089, 2.1001247594196);
    hydrogen(-1.8541623309875, -0.1029264385221, 0.78237823858956);
    hydrogen(-1.0258178277451, 1.3652158493738, 0.56894179293594);
    hydrogen(-0.39545492653663, -0.38422555092091, -1.7565781981979);
    hydrogen(0.7434912085366, 0.88851602455593, -1.6627706717391);
    hydrogen(1.3971715675692, -0.74634733382723, -1.5386642531208);

    numAtoms = atomArray.length;
    orientation = new makeOrientation();
    drawAtoms();
}

function cpeptide() {
    scalar = 0.06;

    atomArray = new Array();
    nitrogen(24.966000, -0.646000, 22.314000);
    carbon(24.121000, 0.549000, 22.271000);
    carbon(24.794000, 1.733000, 22.943000);
    oxygen(25.742000, 1.575000, 23.764000);
    carbon(22.812000, 0.323000, 23.047000);
    carbon(21.763000, 1.415000, 22.695000);
    carbon(20.497000, 1.124000, 23.561000);
    carbon(20.706000, 1.659000, 24.970000);
    nitrogen(21.524000, 0.759000, 25.825000);
    nitrogen(24.300000, 2.909000, 22.632000);
    carbon(24.858000, 4.145000, 23.207000);
    carbon(24.567000, 4.201000, 24.693000);
    oxygen(23.398000, 4.051000, 25.038000);
    carbon(24.238000, 5.355000, 22.537000);
    carbon(24.775000, 6.731000, 22.894000);
    carbon(24.277000, 7.798000, 21.950000);
    oxygen(23.087000, 7.974000, 21.734000);
    oxygen(25.200000, 8.451000, 21.448000);
    nitrogen(25.608000, 4.399000, 25.499000);
    carbon(25.475000, 4.513000, 26.954000);
    carbon(24.803000, 5.847000, 27.263000);
    oxygen(24.805000, 6.756000, 26.419000);
    carbon(26.857000, 4.478000, 27.708000);
    oxygen(27.581000, 5.698000, 27.276000);
    carbon(27.750000, 3.260000, 27.496000);
    nitrogen(24.316000, 6.023000, 28.470000);
    carbon(23.646000, 7.264000, 28.928000);
    carbon(24.622000, 8.442000, 28.958000);
    oxygen(24.267000, 9.606000, 28.686000);
    carbon(23.015000, 7.064000, 30.281000);
    nitrogen(25.824000, 8.089000, 29.315000);
    carbon(26.973000, 9.001000, 29.411000);
    carbon(27.301000, 9.459000, 27.996000);
    oxygen(27.487000, 10.671000, 27.734000);
    carbon(28.136000, 8.252000, 30.019000);
    nitrogen(27.347000, 8.474000, 27.100000);
    carbon(27.667000, 8.723000, 25.675000);
    carbon(26.563000, 9.530000, 25.053000);
    oxygen(26.910000, 10.405000, 24.191000);
    carbon(28.009000, 7.493000, 24.904000);
    nitrogen(25.331000, 9.253000, 25.468000);
    carbon(24.214000, 10.046000, 24.882000);
    carbon(24.248000, 11.484000, 25.368000);
    oxygen(23.864000, 12.449000, 24.637000);
    carbon(22.873000, 9.453000, 25.223000);
    carbon(21.741000, 9.892000, 24.304000);
    carbon(20.430000, 9.673000, 25.048000);
    carbon(19.195000, 9.601000, 24.179000);
    nitrogen(18.362000, 8.506000, 24.926000);
    nitrogen(24.611000, 11.716000, 26.577000);
    carbon(24.684000, 13.122000, 27.093000);
    carbon(25.642000, 13.925000, 26.270000);
    oxygen(25.432000, 15.007000, 25.725000);
    carbon(25.131000, 13.060000, 28.561000);
    carbon(25.203000, 14.394000, 29.198000);
    carbon(24.126000, 14.986000, 29.804000);
    carbon(26.452000, 15.039000, 29.163000);
    carbon(24.280000, 16.270000, 30.378000);
    carbon(26.616000, 16.300000, 29.751000);
    carbon(25.504000, 16.901000, 30.351000);
    nitrogen(26.898000, 13.337000, 26.165000);
    carbon(27.881000, 14.091000, 25.359000);
    carbon(27.371000, 14.464000, 24.013000);
    oxygen(27.476000, 15.538000, 23.451000);
    carbon(29.091000, 13.150000, 25.107000);
    carbon(30.026000, 13.107000, 26.317000);
    carbon(30.913000, 11.894000, 26.266000);
    oxygen(31.790000, 11.714000, 27.007000);
    oxygen(30.618000, 11.126000, 25.332000);
    nitrogen(26.718000, 13.468000, 23.337000);
    carbon(26.217000, 13.615000, 22.008000);
    carbon(25.181000, 14.741000, 21.898000);
    oxygen(25.315000, 15.571000, 20.989000);
    carbon(25.543000, 12.364000, 21.390000);
    carbon(25.041000, 12.649000, 20.020000);
    carbon(24.583000, 11.429000, 19.284000);
    nitrogen(23.705000, 10.574000, 20.090000);
    carbon(22.391000, 10.715000, 20.025000);
    nitrogen(21.597000, 9.973000, 20.783000);
    nitrogen(21.916000, 11.570000, 19.124000);
    nitrogen(24.193000, 14.618000, 22.850000);
    carbon(23.137000, 15.655000, 22.818000);
    carbon(23.542000, 16.942000, 23.484000);
    oxygen(22.862000, 17.924000, 23.011000);
    carbon(21.763000, 15.296000, 23.391000);
    carbon(21.537000, 13.847000, 23.729000);
    carbon(20.051000, 13.594000, 24.035000);
    oxygen(19.210000, 13.643000, 23.132000);
    nitrogen(19.779000, 13.299000, 25.295000);
    nitrogen(24.459000, 17.136000, 24.385000);
    carbon(24.725000, 18.429000, 24.940000);
    carbon(26.072000, 19.049000, 24.947000);
    oxygen(26.138000, 20.232000, 25.394000);
    carbon(24.368000, 18.383000, 26.521000);
    carbon(22.979000, 17.885000, 26.681000);
    nitrogen(21.845000, 18.552000, 26.268000);
    carbon(22.549000, 16.744000, 27.261000);
    carbon(20.780000, 17.885000, 26.588000);
    nitrogen(21.188000, 16.738000, 27.201000);
    nitrogen(27.098000, 18.382000, 24.448000);
    carbon(28.468000, 18.994000, 24.490000);
    carbon(28.829000, 19.578000, 23.143000);
    oxygen(28.604000, 18.999000, 22.059000);
    carbon(29.418000, 17.918000, 24.907000);
    carbon(29.453000, 17.361000, 26.285000);
    sulfur(29.520000, 18.746000, 27.500000);
    carbon(31.108000, 19.501000, 27.016000);
    oxygen(29.395000, 20.806000, 23.182000);
    numAtoms = atomArray.length;

    // center the molecule around the (0,0,0) point
    var xsum = 0.0;
    var ysum = 0.0;
    var zsum = 0.0;
    for (var i = 0; i < numAtoms; i++) {
	pos = atomArray[i].getPos();
	xsum += pos.getX();
	ysum += pos.getY();
	zsum += pos.getZ();
    }
    var v = new makeVector(-xsum / numAtoms, -ysum / numAtoms, -zsum / numAtoms);
    for (i = 0; i < numAtoms; i++) {
	atomArray[i] = atomArray[i].nudge(v);
    }    

    orientation = new makeOrientation();
    drawAtoms();
}

function makeMatrix(x00,x01,x02,x10,x11,x12,x20,x21,x22) {
    this.toString = function() {
	return "[matrix " +
	x00 + " " + x01 + " " + x02 + " " +
	x10 + " " + x11 + " " + x12 + " " +
	x20 + " " + x21 + " " + x22 + "]";
    }
    this.get = function(i,j) {
	switch (i) {
	    case 0:
	    switch (j) {
		case 0:  return x00;
		case 1:  return x01;
		default: return x02;
	    }
	    case 1:
	    switch (j) {
		case 0:  return x10;
		case 1:  return x11;
		default: return x12;
	    }
	    default:
	    switch (j) {
		case 0:  return x20;
		case 1:  return x21;
		default: return x22;
	    }
	}
    }
    this.set = function(i,j,value) {
	switch (i) {
	    case 0:
	    switch (j) {
		case 0:  x00 = value; return;
		case 1:  x01 = value; return;
		default: x02 = value; return;
	    }
	    case 1:
	    switch (j) {
		case 0:  x10 = value; return;
		case 1:  x11 = value; return;
		default: x12 = value; return;
	    }
	    default:
	    switch (j) {
		case 0:  x20 = value; return;
		case 1:  x21 = value; return;
		default: x22 = value; return;
	    }
	}
    }
    this.timesVector = function(v) {
	v = new makeVector(x00 * v.getX() + x01 * v.getY() + x02 * v.getZ(),
			   x10 * v.getX() + x11 * v.getY() + x12 * v.getZ(),
			   x20 * v.getX() + x21 * v.getY() + x22 * v.getZ());
	return v;
    }
    this.timesMatrix = function(m) {
	return new makeMatrix(x00 * m.get(0,0) + x01 * m.get(1,0) + x02 * m.get(2,0),
			      x00 * m.get(0,1) + x01 * m.get(1,1) + x02 * m.get(2,1),
			      x00 * m.get(0,2) + x01 * m.get(1,2) + x02 * m.get(2,2),
			      x10 * m.get(0,0) + x11 * m.get(1,0) + x12 * m.get(2,0),
			      x10 * m.get(0,1) + x11 * m.get(1,1) + x12 * m.get(2,1),
			      x10 * m.get(0,2) + x11 * m.get(1,2) + x12 * m.get(2,2),
			      x20 * m.get(0,0) + x21 * m.get(1,0) + x22 * m.get(2,0),
			      x20 * m.get(0,1) + x21 * m.get(1,1) + x22 * m.get(2,1),
			      x20 * m.get(0,2) + x21 * m.get(1,2) + x22 * m.get(2,2));
    }
    this.renormalize = function() {
	renorm = function(i1,j1,i2,j2,i3,j3) {
	    x = this.get(i1,j1);
	    y = this.get(i2,j2);
	    z = this.get(i3,j3);
	    k = 1 / Math.sqrt(x * x + y * y + z * z);
	    this.set(i1,j1,k * x);
	    this.set(i2,j2,k * y);
	    this.set(i3,j3,k * z);
	}
	renorm(0,0,0,1,0,2); renorm(1,0,1,1,1,2); renorm(2,0,2,1,2,2);
	renorm(0,0,1,0,2,0); renorm(0,1,1,1,2,1); renorm(0,2,1,2,2,2);
    }
}

function makeOrientation() {
    this.matrix = new makeMatrix(1,0,0, 0,1,0, 0,0,1);
    this.rotcount = 0;
    this.toString = function() {
	return "[ orientation " + this.matrix.toString() + " ]";
    }
    renormalize = function() {
	// every 100th rotation, renormalize the matrix
	if (this.rotcount < 100) {
	    this.rotcount++;
	    return;
	}
	this.rotcount = 0;
	this.matrix.renormalize();
    }
    this.rotX = function(angle) {
	var c = Math.cos(angle);
	var s = Math.sin(angle);
	var m = new makeMatrix(1,0,0, 0,c,s, 0,-s,c);
	this.matrix = m.timesMatrix(this.matrix);
	// this.renormalize();
    }
    this.rotY = function(angle) {
	var c = Math.cos(angle);
	var s = Math.sin(angle);
	var m = new makeMatrix(c,0,s, 0,1,0, -s,0,c);
	this.matrix = m.timesMatrix(this.matrix);
	// this.renormalize();
    }
    // perspective factor isn't valid for a vector unless you've run
    // the transform method on that vector, and not yet run it on any other
    this.P = 1;
    this.transform = function(vec) {
	vec = this.matrix.timesVector(vec);
	var perspectiveDistance = 3;
	this.P = perspectiveDistance / (perspectiveDistance - vec.getZ());
	return new makeVector(this.P * vec.getX(), this.P * vec.getY(), vec.getZ());
    }
}

function showTab() {
    var selectedId = getHash( this.getAttribute('href') );
    currentMode = selectedId;
    for ( var id in contentDivs ) {
	if ( id == selectedId ) {
	    tabLinks[id].className = 'selected';
	    contentDivs[id].className = 'tabContent';
	} else {
	    tabLinks[id].className = '';
	    contentDivs[id].className = 'tabContent hide';
	}
    }
    return false;
}

function getFirstChildWithTagName( element, tagName ) {
    for ( var i = 0; i < element.childNodes.length; i++ ) {
	if ( element.childNodes[i].nodeName == tagName ) return element.childNodes[i];
    }
}

function getHash( url ) {
    var hashPos = url.lastIndexOf ( '#' );
    return url.substring( hashPos + 1 );
}

function getEvent(e) {
    // IE doesn't pass the event object
    if (e == null) e = window.event;
    // IE uses srcElement, others use target
    return (e.target != null) ? e.target : e.srcElement;
}

function mouseDown(e) {
    var target = getEvent(e);
    if (currentMode == "view") {
        if (target.id == "canvas") {
            mouseIsDown = true;
            previousX = e.clientX;
            previousY = e.clientY;
        }
        if (numAtoms < FAST_DRAW_THRESHOLD)
            determineBonds();
        else
            determineBoundingBox();
    }
}

function mouseUp() {
    mouseIsDown = false;
    if (currentMode == "view") {
        drawAtoms();
    }
}

function mouseMove(e) {
    if (mouseIsDown && currentMode == "view") {
	var target = getEvent(e);
	if (target.id == "canvas") {
	    var newX = e.clientX;
	    var newY = e.clientY;
	    var theta = 0.02 * (newX - previousX);
	    var phi = 0.02 * (newY - previousY);
	    previousX = newX;
	    previousY = newY;
	    orientation.rotX(phi);
	    orientation.rotY(theta);
	    drawFast();
	}
    }
}
