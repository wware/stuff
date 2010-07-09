/**
 * diamrod.java
 * Copyright (c) 1997,1998,1999 Will Ware, all rights reserved.
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

public class diamrod extends group {
	public static final String rcsid =
	    "$Id: diamrod.java,v 1.1 1999/01/18 03:51:57 wware Exp $";
	public diamrod(Panel p) {
		mypanel = p;
		addAtom(new carbon(atom.SP3, -4.34738, 0.184623, 1.30738));
		addAtom(new
			carbon(atom.SP3, -1.87113, 0.0850365, 1.28317));
		addAtom(new
			carbon(atom.SP3, 0.602237, -0.0505073, 1.31561));
		addAtom(new carbon(atom.SP3, 3.08786, -0.204393, 1.35278));
		addAtom(new carbon(atom.SP3, 5.57168, -0.354634, 1.34363));
		addAtom(new carbon(atom.SP3, 8.05083, -0.547865, 1.36062));
		addAtom(new carbon(atom.SP3, 6.75071, -1.26262, 1.02126));
		addAtom(new carbon(atom.SP3, 4.27976, -1.09377, 1.02225));
		addAtom(new carbon(atom.SP3, 1.79877, -0.940989, 1.01019));
		addAtom(new
			carbon(atom.SP3, -0.675419, -0.798359, 0.964786));
		addAtom(new
			carbon(atom.SP3, -3.14481, -0.678664, 0.953939));
		addAtom(new carbon(atom.SP3, -6.84513, 0.275513, 1.31319));
		addAtom(new
			carbon(atom.SP3, -5.62639, -0.582848, 0.995201));
		addAtom(new hydrogen(-6.85938, 0.521999, 2.39896));
		addAtom(new hydrogen(-4.31909, 0.435946, 2.39122));
		addAtom(new hydrogen(-1.86474, 0.345574, 2.36495));
		addAtom(new hydrogen(0.592448, 0.207833, 2.39836));
		addAtom(new hydrogen(3.09814, 0.0443067, 2.43809));
		addAtom(new hydrogen(5.58882, -0.0881504, 2.42387));
		addAtom(new hydrogen(8.06475, -0.294321, 2.44406));
		addAtom(new hydrogen(8.90936, -1.21532, 1.12321));
		addAtom(new hydrogen(-5.66285, -1.51193, 1.60819));
		addAtom(new hydrogen(-3.1776, -1.6206, 1.54544));
		addAtom(new hydrogen(-0.735558, -1.73481, 1.56332));
		addAtom(new hydrogen(1.73158, -1.87351, 1.61414));
		addAtom(new hydrogen(4.22683, -2.02523, 1.62916));
		addAtom(new hydrogen(6.66969, -2.19548, 1.62228));
		addAtom(new
			carbon(atom.SP3, -5.63656, -0.936323, -0.48684));
		addAtom(new
			carbon(atom.SP3, -3.16047, -1.01819, -0.530717));
		addAtom(new
			carbon(atom.SP3, -0.684416, -1.14326, -0.518294));
		addAtom(new
			carbon(atom.SP3, 1.78941, -1.29194, -0.472398));
		addAtom(new
			carbon(atom.SP3, 4.26341, -1.44955, -0.458775));
		addAtom(new
			carbon(atom.SP3, 6.73814, -1.61367, -0.460864));
		addAtom(new carbon(atom.SP3, -6.77986, 1.56149, 0.499002));
		addAtom(new carbon(atom.SP3, -4.29154, 1.46565, 0.485639));
		addAtom(new carbon(atom.SP3, -1.80229, 1.36045, 0.453044));
		addAtom(new carbon(atom.SP3, 0.685318, 1.22472, 0.486549));
		addAtom(new carbon(atom.SP3, 3.17519, 1.07462, 0.529136));
		addAtom(new carbon(atom.SP3, 5.66426, 0.920036, 0.515707));
		addAtom(new carbon(atom.SP3, 8.1516, 0.727405, 0.534469));
		addAtom(new carbon(atom.SP3, 8.1417, 0.365132, -0.944634));
		addAtom(new carbon(atom.SP3, 6.83379, -0.33758, -1.28589));
		addAtom(new carbon(atom.SP3, 5.64647, 0.561874, -0.96438));
		addAtom(new
			carbon(atom.SP3, 4.35069, -0.172109, -1.28296));
		addAtom(new carbon(atom.SP3, 3.1632, 0.720789, -0.951496));
		addAtom(new
			carbon(atom.SP3, 1.8771, -0.0160702, -1.29797));
		addAtom(new
			carbon(atom.SP3, 0.678692, 0.872479, -0.994806));
		addAtom(new
			carbon(atom.SP3, -0.608696, 0.134527, -1.34301));
		addAtom(new carbon(atom.SP3, -1.81609, 1.01112, -1.02953));
		addAtom(new carbon(atom.SP3, -3.1052, 0.26406, -1.35131));
		addAtom(new carbon(atom.SP3, -4.31456, 1.11968, -0.99657));
		addAtom(new carbon(atom.SP3, -5.58565, 0.343867, -1.3097));
		addAtom(new
			carbon(atom.SP3, -6.79216, 1.21077, -0.982998));
		addAtom(new hydrogen(6.82272, -0.592054, -2.36927));
		addAtom(new hydrogen(4.32551, -0.43034, -2.3651));
		addAtom(new hydrogen(1.88283, -0.277257, -2.37953));
		addAtom(new hydrogen(-0.606199, -0.121707, -2.42623));
		addAtom(new hydrogen(-3.13474, 0.0226246, -2.4378));
		addAtom(new hydrogen(-5.60075, 0.0833445, -2.39143));
		addAtom(new hydrogen(5.70336, 1.4904, -1.57606));
		addAtom(new hydrogen(3.22674, 1.65673, -1.54998));
		addAtom(new hydrogen(0.747644, 1.80537, -1.59786));
		addAtom(new hydrogen(-1.76558, 1.94483, -1.63295));
		addAtom(new hydrogen(-4.30083, 2.06065, -1.59079));
		addAtom(new hydrogen(-6.74753, 2.14609, -1.58399));
		addAtom(new hydrogen(7.60599, -2.27031, -0.693731));
		addAtom(new
			carbon(atom.SP3, 2.97586, -2.18392, -0.801717));
		addAtom(new carbon(atom.SP3, 5.4466, -2.34596, -0.793592));
		addAtom(new hydrogen(2.96198, -2.41807, -1.8897));
		addAtom(new hydrogen(2.9086, -3.12802, -0.216226));
		addAtom(new hydrogen(5.42702, -2.59385, -1.87846));
		addAtom(new hydrogen(5.38516, -3.28302, -0.196466));
		addAtom(new
			carbon(atom.SP3, 0.509467, -2.03065, -0.832792));
		addAtom(new carbon(atom.SP3, -1.9619, -1.89502, -0.85817));
		addAtom(new
			carbon(atom.SP3, -4.43133, -1.79068, -0.846329));
		addAtom(new hydrogen(0.439702, -2.9713, -0.242352));
		addAtom(new hydrogen(-2.01387, -2.82545, -0.250173));
		addAtom(new hydrogen(-4.46254, -2.02869, -1.93335));
		addAtom(new hydrogen(-4.44993, -2.73374, -0.256098));
		addAtom(new hydrogen(-1.96895, -2.15313, -1.94057));
		addAtom(new hydrogen(0.515438, -2.27084, -1.91946));
		addAtom(new hydrogen(-7.65935, 2.19993, 0.738965));
		addAtom(new carbon(atom.SP3, -5.50144, 2.32722, 0.82217));
		addAtom(new carbon(atom.SP3, -3.00782, 2.23103, 0.783358));
		addAtom(new
			carbon(atom.SP3, -0.517637, 2.11177, 0.780577));
		addAtom(new carbon(atom.SP3, 1.9738, 1.9643, 0.827083));
		addAtom(new carbon(atom.SP3, 4.47234, 1.81383, 0.840535));
		addAtom(new carbon(atom.SP3, 6.96853, 1.63642, 0.845393));
		addAtom(new
			carbon(atom.SP3, -6.8979, -1.71566, -0.827929));
		addAtom(new
			carbon(atom.SP3, -8.11424, -0.491396, 0.957602));
		addAtom(new
			carbon(atom.SP3, -8.05065, 0.432628, -1.34096));
		addAtom(new
			carbon(atom.SP3, -8.11132, -0.850164, -0.522163));
		addAtom(new hydrogen(-6.88435, -1.97915, -1.90907));
		addAtom(new hydrogen(-6.93698, -2.64916, -0.222252));
		addAtom(new hydrogen(-9.0383, -1.41143, -0.776215));
		addAtom(new hydrogen(-8.02177, 0.178124, -2.42403));
		addAtom(new hydrogen(-8.94791, 1.05464, -1.1258));
		addAtom(new hydrogen(-9.00614, 0.139066, 1.17056));
		addAtom(new hydrogen(-8.16611, -1.42375, 1.56324));
		addAtom(new hydrogen(-5.48058, 2.59371, 1.9029));
		addAtom(new hydrogen(-2.97639, 3.15156, 0.158385));
		addAtom(new hydrogen(-0.45756, 3.02702, 0.149895));
		addAtom(new hydrogen(2.03872, 2.88558, 0.206136));
		addAtom(new hydrogen(4.49904, 2.09206, 1.91771));
		addAtom(new hydrogen(7.05005, 2.56445, 0.236564));
		addAtom(new hydrogen(8.25053, 1.28748, -1.55673));
		addAtom(new hydrogen(-5.46662, 3.259, 0.214341));
		addAtom(new hydrogen(-2.97782, 2.50968, 1.86002));
		addAtom(new hydrogen(-0.516611, 2.39742, 1.85608));
		addAtom(new hydrogen(6.98202, 1.90035, 1.92646));
		addAtom(new hydrogen(1.96871, 2.24114, 1.90499));
		addAtom(new hydrogen(4.52537, 2.7375, 0.221516));
		addAtom(new hydrogen(9.10092, 1.25339, 0.781044));
		addAtom(new hydrogen(8.99148, -0.321375, -1.15989));
		addBond(12, 11);
		addBond(12, 0);
		addBond(0, 10);
		addBond(10, 1);
		addBond(1, 9);
		addBond(9, 2);
		addBond(2, 8);
		addBond(8, 3);
		addBond(3, 7);
		addBond(7, 4);
		addBond(4, 6);
		addBond(6, 5);
		addBond(1, 15);
		addBond(0, 14);
		addBond(11, 13);
		addBond(2, 16);
		addBond(3, 17);
		addBond(5, 19);
		addBond(4, 18);
		addBond(20, 5);
		addBond(12, 21);
		addBond(10, 22);
		addBond(9, 23);
		addBond(8, 24);
		addBond(25, 7);
		addBond(26, 6);
		addBond(27, 12);
		addBond(28, 10);
		addBond(29, 9);
		addBond(30, 8);
		addBond(31, 7);
		addBond(32, 6);
		addBond(33, 11);
		addBond(34, 0);
		addBond(35, 1);
		addBond(36, 2);
		addBond(37, 3);
		addBond(38, 4);
		addBond(39, 5);
		addBond(32, 41);
		addBond(31, 43);
		addBond(30, 45);
		addBond(29, 47);
		addBond(28, 49);
		addBond(27, 51);
		addBond(33, 52);
		addBond(34, 50);
		addBond(35, 48);
		addBond(36, 46);
		addBond(37, 44);
		addBond(38, 42);
		addBond(39, 40);
		addBond(40, 41);
		addBond(41, 42);
		addBond(42, 43);
		addBond(43, 44);
		addBond(44, 45);
		addBond(45, 46);
		addBond(46, 47);
		addBond(47, 48);
		addBond(48, 49);
		addBond(49, 50);
		addBond(50, 51);
		addBond(51, 52);
		addBond(55, 45);
		addBond(54, 43);
		addBond(53, 41);
		addBond(56, 47);
		addBond(57, 49);
		addBond(58, 51);
		addBond(59, 42);
		addBond(60, 44);
		addBond(61, 46);
		addBond(62, 48);
		addBond(63, 50);
		addBond(64, 52);
		addBond(65, 32);
		addBond(67, 32);
		addBond(66, 31);
		addBond(67, 31);
		addBond(66, 30);
		addBond(71, 67);
		addBond(70, 67);
		addBond(69, 66);
		addBond(68, 66);
		addBond(27, 74);
		addBond(28, 73);
		addBond(29, 72);
		addBond(30, 72);
		addBond(29, 73);
		addBond(28, 74);
		addBond(72, 75);
		addBond(72, 80);
		addBond(73, 76);
		addBond(73, 79);
		addBond(74, 78);
		addBond(74, 77);
		addBond(81, 33);
		addBond(82, 33);
		addBond(82, 34);
		addBond(39, 87);
		addBond(38, 87);
		addBond(34, 83);
		addBond(83, 35);
		addBond(35, 84);
		addBond(84, 36);
		addBond(36, 85);
		addBond(85, 37);
		addBond(37, 86);
		addBond(86, 38);
		addBond(90, 52);
		addBond(89, 11);
		addBond(88, 27);
		addBond(91, 88);
		addBond(91, 89);
		addBond(91, 90);
		addBond(94, 91);
		addBond(92, 88);
		addBond(93, 88);
		addBond(96, 90);
		addBond(95, 90);
		addBond(98, 89);
		addBond(97, 89);
		addBond(99, 82);
		addBond(100, 83);
		addBond(101, 84);
		addBond(102, 85);
		addBond(103, 86);
		addBond(104, 87);
		addBond(105, 40);
		addBond(106, 82);
		addBond(107, 83);
		addBond(108, 84);
		addBond(109, 87);
		addBond(111, 86);
		addBond(110, 85);
		addBond(113, 40);
		addBond(112, 39);
	}
}
