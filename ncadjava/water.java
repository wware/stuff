/**
 * water.java
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

public class water extends group {
	public static final String rcsid =
	    "$Id: water.java,v 1.2 1999/01/12 04:10:16 wware Exp $";
	public water(Panel p) {
		mypanel = p;
		addAtom(new oxygen(atom.SP3,
				   -2.3179271590246, -1.245842432839,
				   -3.225993108704));
		addAtom(new
			oxygen(atom.SP3, 2.3592808043861, 1.2883138211754,
			       -4.0566716208121));
		addAtom(new
			oxygen(atom.SP3, 3.4464918152871, -2.3984359834831,
			       -1.3053134766819));
		addAtom(new
			oxygen(atom.SP3, 0.92385048777157,
			       -0.53954091389895, 3.7928522696704));
		addAtom(new
			oxygen(atom.SP3, -4.2677459289202, 1.9193549862933,
			       4.356427009362));
		addAtom(new
			hydrogen(1.0789273562621, 1.3419090146687,
				 -4.3584421728616));
		addAtom(new
			hydrogen(3.245743080874, 2.2672242711381,
				 -4.1464906605444));
		addAtom(new
			hydrogen(2.3516554179415, -1.6877883462116,
				 -1.414699257383));
		addAtom(new
			hydrogen(4.584157024776, -2.0385189650844,
				 -0.71848866494464));
		addAtom(new
			hydrogen(2.2164881262824, -0.39165908867432,
				 3.6347739960268));
		addAtom(new
			hydrogen(0.30142049535067, -1.4851276405958,
				 4.4894810725234));
		addAtom(new
			hydrogen(-3.628563270266, -1.2525119524143,
				 -3.2705210501593));
		addAtom(new
			hydrogen(-1.5480820293511, -0.74756572413648,
				 -2.264618871057));
		addAtom(new
			hydrogen(-3.2418205250508, 2.7491808852943,
				 4.4662953756944));
		addAtom(new
			hydrogen(-5.5038756963187, 2.2210080687682,
				 4.0214091598709));
		addBond(4, 14);
		addBond(13, 4);
		addBond(0, 11);
		addBond(12, 0);
		addBond(3, 10);
		addBond(9, 3);
		addBond(2, 8);
		addBond(7, 2);
		addBond(1, 6);
		addBond(5, 1);
	}
}
