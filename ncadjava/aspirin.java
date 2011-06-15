/**
 * aspirin.java
 * Copyright (c) 1997,1998 Will Ware, all rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and other materials provided with the distribution.
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

public class aspirin extends group {
	public static final String rcsid =
	    "$Id: aspirin.java,v 1.7 1999/01/14 04:37:18 wware Exp $";
	public aspirin(Panel p) {
		mypanel = p;
		addAtom(new carbon(atom.SP2, 1.83608, 1.22083, 1.53739));
		addAtom(new carbon(atom.SP2, 1.28975, 0.0460942, 1.06565));
		addAtom(new
			carbon(atom.SP2, 0.387841, 0.0591862, 0.00575193));
		addAtom(new
			carbon(atom.SP2, 0.034792, 1.27928, -0.593068));
		addAtom(new carbon(atom.SP2, 0.619637, 2.4537, -0.111554));
		addAtom(new carbon(atom.SP2, 1.50751, 2.42367, 0.946671));
		addAtom(new
			carbon(atom.SP2, -1.12674, -1.82836, 0.134445));
		addAtom(new
			carbon(atom.SP3, -0.874563, -3.27085, 0.404819));
		addAtom(new
			carbon(atom.SP2, -0.868666, 1.30931, -1.59738));
		addAtom(new oxygen(atom.SP2, -1.20263, 2.33819, -2.18052));
		addAtom(new oxygen(atom.SP3, -1.54607, 0.232238, -2.0684));
		addAtom(new
			oxygen(atom.SP3, -0.105358, -1.13034, -0.430703));
		addAtom(new oxygen(atom.SP2, -2.27455, -1.4162, 0.023975));
		addAtom(new hydrogen(0.153648, -3.37238, 0.817824));
		addAtom(new hydrogen(-1.62316, -3.65471, 1.13198));
		addAtom(new hydrogen(-0.949359, -3.82301, -0.557651));
		addAtom(new hydrogen(-1.66805, 0.0477654, -3.2422));
		addAtom(new hydrogen(0.393079, 3.44198, -0.53912));
		addAtom(new hydrogen(1.94648, 3.35794, 1.3223));
		addAtom(new hydrogen(2.52943, 1.19885, 2.38912));
		addAtom(new hydrogen(1.54092, -0.91316, 1.54067));
		addBond(7, 15);
		addBond(7, 14);
		addBond(7, 13);
		addBond(6, 7);
		addBond(11, 6);
		addBond(2, 11);
		addBond(1, 2);
		addBond(20, 1);
		addBond(10, 16);
		addBond(8, 10);
		addBond(3, 8);
		addBond(4, 3);
		addBond(17, 4);
		addBond(5, 18);
		addBond(0, 5);
		addBond(19, 0);
		addBond(8, 9, 2);
		addBond(6, 12, 2);
		addBond(2, 3, 2);
		addBond(5, 4, 2);
		addBond(0, 1, 2);
	}
}
