/**
 * propane.java
 * Copyright (c) 1997 Will Ware, all rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    or its derived works must display the following acknowledgement:
 * 	This product includes software developed by Will Ware.
 * 
 * This software is provided "as is" and any express or implied warranties,
 * including, but not limited to, the implied warranties of merchantability
 * or fitness for any particular purpose are disclaimed. In no event shall
 * Will Ware be liable for any direct, indirect, incidental, special,
 * exemplary, or consequential damages (including, but not limited to,
 * procurement of substitute goods or services; loss of use, data, or
 * profits; or business interruption) however caused and on any theory of
 * liability, whether in contract, strict liability, or tort (including
 * negligence or otherwise) arising in any way out of the use of this
 * software, even if advised of the possibility of such damage.
 */

import java.awt.Panel;

public class propane extends group {
	public static final String rcsid =
	    "$Id: propane.java,v 1.2 1999/01/12 04:09:36 wware Exp $";
	public propane(Panel p) {
		mypanel = p;
		addAtom(new carbon(atom.SP3,
				   -0.85748833891984, 0.34794546061805,
				   0.98214417192606));
		addAtom(new
			carbon(atom.SP3, 0.4065315061009,
			       -0.28637730259981, 0.32481170697201));
		addAtom(new
			carbon(atom.SP3, 0.53857043456917,
			       -0.1123325081226, -1.2159381954418));
		addAtom(new
			hydrogen(1.3229946018417, 0.041931101228472,
				 0.86981518860896));
		addAtom(new
			hydrogen(0.53148792074281, -1.3725391912445,
				 0.54573546004739));
		addAtom(new
			hydrogen(-0.80732381517125, 0.36113988946089,
				 2.1001247594196));
		addAtom(new
			hydrogen(-1.8541623309875, -0.1029264385221,
				 0.78237823858956));
		addAtom(new
			hydrogen(-1.0258178277451, 1.3652158493738,
				 0.56894179293594));
		addAtom(new
			hydrogen(-0.39545492653663, -0.38422555092091,
				 -1.7565781981979));
		addAtom(new
			hydrogen(0.7434912085366, 0.88851602455593,
				 -1.6627706717391));
		addAtom(new
			hydrogen(1.3971715675692, -0.74634733382723,
				 -1.5386642531208));
		addBond(3, 1);
		addBond(1, 4);
		addBond(2, 10);
		addBond(2, 9);
		addBond(2, 8);
		addBond(1, 2);
		addBond(0, 1);
		addBond(0, 7);
		addBond(0, 6);
		addBond(5, 0);
	}
}
