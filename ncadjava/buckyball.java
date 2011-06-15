/**
 * buckyball.java
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

public class buckyball extends group {
	public static final String rcsid =
	    "$Id: buckyball.java,v 1.3 1999/01/15 03:41:47 wware Exp $";
	public buckyball(Panel p) {
		mypanel = p;
		addAtom(new
			carbon(atom.SP2, 0.0573549, 1.64498, -3.04102));
		addAtom(new carbon(atom.SP2, 1.03176, 2.32125, -2.34332));
		addAtom(new carbon(atom.SP2, 2.13902, 1.50934, -2.25809));
		addAtom(new carbon(atom.SP2, 1.84578, 0.327267, -2.90059));
		addAtom(new carbon(atom.SP2, 0.559893, 0.413392, -3.3816));
		addAtom(new carbon(atom.SP2, 0.671058, 3.15769, -1.2398));
		addAtom(new carbon(atom.SP2, -1.31902, 1.77787, -2.66501));
		addAtom(new
			carbon(atom.SP2, -0.289449, -0.738496, -3.36309));
		addAtom(new carbon(atom.SP2, 2.3364, -0.912276, -2.3782));
		addAtom(new carbon(atom.SP2, 2.93217, 1.49684, -1.0657));
		addAtom(new carbon(atom.SP2, 1.43499, 3.1485, -0.092806));
		addAtom(new
			carbon(atom.SP2, 2.58709, 2.30005, -0.00372734));
		addAtom(new
			carbon(atom.SP2, 3.40295, 0.305229, -0.563633));
		addAtom(new
			carbon(atom.SP2, 3.09884, -0.923131, -1.23323));
		addAtom(new carbon(atom.SP2, 1.52099, -2.01988, -2.35857));
		addAtom(new carbon(atom.SP2, 0.182172, -1.9322, -2.86217));
		addAtom(new
			carbon(atom.SP2, -1.61054, -0.612941, -2.99995));
		addAtom(new
			carbon(atom.SP2, -2.13756, 0.671754, -2.64451));
		addAtom(new carbon(atom.SP2, -1.66688, 2.58296, -1.6025));
		addAtom(new
			carbon(atom.SP2, -0.649991, 3.28543, -0.875809));
		addAtom(new
			carbon(atom.SP2, -2.69886, 1.97024, -0.928035));
		addAtom(new carbon(atom.SP2, -2.99364, 0.79205, -1.57278));
		addAtom(new
			carbon(atom.SP2, -1.95414, -1.72469, -2.27069));
		addAtom(new
			carbon(atom.SP2, -0.848623, -2.5407, -2.18575));
		addAtom(new carbon(atom.SP2, 1.77646, -2.71407, -1.20098));
		addAtom(new
			carbon(atom.SP2, 2.75213, -2.03926, -0.503501));
		addAtom(new carbon(atom.SP2, 3.34984, 0.371733, 0.811682));
		addAtom(new carbon(atom.SP2, 2.84221, 1.6046, 1.15354));
		addAtom(new carbon(atom.SP2, 0.582252, 3.26479, 0.979406));
		addAtom(new
			carbon(atom.SP2, -0.703704, 3.34825, 0.494924));
		addAtom(new
			carbon(atom.SP2, 0.704059, -3.35089, -0.497321));
		addAtom(new
			carbon(atom.SP2, -0.582013, -3.26514, -0.979397));
		addAtom(new
			carbon(atom.SP2, -2.84206, -1.60217, -1.15432));
		addAtom(new
			carbon(atom.SP2, -3.35218, -0.370296, -0.811414));
		addAtom(new carbon(atom.SP2, 2.69642, -1.97122, 0.926123));
		addAtom(new carbon(atom.SP2, 2.99142, -0.789235, 1.57172));
		addAtom(new carbon(atom.SP2, 1.95601, 1.72785, 2.27151));
		addAtom(new carbon(atom.SP2, 0.848919, 2.54052, 2.18579));
		addAtom(new carbon(atom.SP2, -1.77704, 2.71338, 1.19864));
		addAtom(new carbon(atom.SP2, -2.75311, 2.03706, 0.502776));
		addAtom(new
			carbon(atom.SP2, -2.58563, -2.30075, 0.00576535));
		addAtom(new
			carbon(atom.SP2, -1.43248, -3.14731, 0.095186));
		addAtom(new carbon(atom.SP2, 0.64914, -3.28612, 0.8776));
		addAtom(new carbon(atom.SP2, 1.66536, -2.58169, 1.60315));
		addAtom(new carbon(atom.SP2, 2.13733, -0.670769, 2.64407));
		addAtom(new carbon(atom.SP2, 1.60962, 0.612496, 3.00221));
		addAtom(new carbon(atom.SP2, -0.18104, 1.92912, 2.86189));
		addAtom(new carbon(atom.SP2, -1.52014, 2.01836, 2.35998));
		addAtom(new carbon(atom.SP2, -3.09674, 0.923776, 1.23057));
		addAtom(new
			carbon(atom.SP2, -3.40136, -0.305317, 0.560446));
		addAtom(new
			carbon(atom.SP2, -0.672217, -3.15728, 1.23965));
		addAtom(new carbon(atom.SP2, 0.28808, 0.73933, 3.36334));
		addAtom(new carbon(atom.SP2, 1.32007, -1.77843, 2.66291));
		addAtom(new carbon(atom.SP2, -2.93134, -1.49668, 1.06596));
		addAtom(new carbon(atom.SP2, -2.33625, 0.911869, 2.37768));
		addAtom(new
			carbon(atom.SP2, -0.561572, -0.412517, 3.38415));
		addAtom(new
			carbon(atom.SP2, -0.0558127, -1.64732, 3.04243));
		addAtom(new carbon(atom.SP2, -1.03124, -2.32114, 2.34482));
		addAtom(new carbon(atom.SP2, -2.13812, -1.50736, 2.25944));
		addAtom(new
			carbon(atom.SP2, -1.84704, -0.328694, 2.90015));
		addBond(57, 50, 2);
		addBond(52, 56, 2);
		addBond(55, 51, 2);
		addBond(54, 59, 2);
		addBond(53, 58, 2);
		addBond(59, 55);
		addBond(58, 59);
		addBond(57, 58);
		addBond(56, 57);
		addBond(55, 56);
		addBond(47, 54);
		addBond(48, 54);
		addBond(53, 49);
		addBond(40, 53);
		addBond(52, 43);
		addBond(44, 52);
		addBond(51, 45);
		addBond(46, 51);
		addBond(50, 41);
		addBond(42, 50);
		addBond(49, 33);
		addBond(39, 48);
		addBond(49, 48, 2);
		addBond(42, 43, 2);
		addBond(45, 44, 2);
		addBond(47, 46, 2);
		addBond(47, 38);
		addBond(37, 46);
		addBond(45, 36);
		addBond(35, 44);
		addBond(43, 34);
		addBond(30, 42);
		addBond(40, 41, 2);
		addBond(40, 32);
		addBond(31, 41);
		addBond(38, 39);
		addBond(39, 20, 2);
		addBond(29, 38, 2);
		addBond(28, 37, 2);
		addBond(27, 36, 2);
		addBond(26, 35, 2);
		addBond(25, 34, 2);
		addBond(37, 36);
		addBond(34, 35);
		addBond(21, 33, 2);
		addBond(22, 32, 2);
		addBond(31, 23, 2);
		addBond(24, 30, 2);
		addBond(32, 33);
		addBond(30, 31);
		addBond(29, 19);
		addBond(28, 29);
		addBond(10, 28);
		addBond(27, 11);
		addBond(26, 27);
		addBond(12, 26);
		addBond(25, 13);
		addBond(24, 25);
		addBond(14, 24);
		addBond(23, 15);
		addBond(22, 23);
		addBond(16, 22);
		addBond(21, 17);
		addBond(20, 21);
		addBond(18, 20);
		addBond(9, 11);
		addBond(12, 9);
		addBond(8, 13);
		addBond(14, 8);
		addBond(7, 15);
		addBond(16, 7);
		addBond(6, 17);
		addBond(18, 6);
		addBond(5, 19);
		addBond(10, 5);
		addBond(3, 8, 2);
		addBond(4, 7, 2);
		addBond(6, 0, 2);
		addBond(19, 18, 2);
		addBond(16, 17, 2);
		addBond(14, 15, 2);
		addBond(12, 13, 2);
		addBond(10, 11, 2);
		addBond(2, 9, 2);
		addBond(1, 5, 2);
		addBond(4, 0);
		addBond(3, 4);
		addBond(2, 3);
		addBond(1, 2);
		addBond(0, 1);
	}
}
