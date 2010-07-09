/**
 * diamond.java
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

public class diamond extends group {
	public static final String rcsid =
	    "$Id: diamond.java,v 1.3 1999/01/18 04:40:12 wware Exp $";
	public diamond(Panel p) {
		mypanel = p;
		addAtom(new carbon(atom.SP3, 2.49072, 2.00042, -1.95816));
		addAtom(new carbon(atom.SP3, 1.00718, 2.34614, -1.93295));
		addAtom(new carbon(atom.SP3, 2.66141, 0.487382, -1.96724));
		addAtom(new carbon(atom.SP3, 3.16363, 2.5659, -0.714823));
		addAtom(new carbon(atom.SP3, 2.51208, 1.97329, 0.530155));
		addAtom(new carbon(atom.SP3, 1.02896, 2.31613, 0.552131));
		addAtom(new
			carbon(atom.SP3, 0.360797, 1.74618, -0.690453));
		addAtom(new carbon(atom.SP3, 2.67988, 0.459003, 0.521737));
		addAtom(new
			carbon(atom.SP3, 0.532365, 0.232441, -0.701677));
		addAtom(new
			carbon(atom.SP3, 1.31044, -0.207805, 0.531389));
		addAtom(new
			carbon(atom.SP3, 3.4419, 0.0537961, -0.732756));
		addAtom(new carbon(atom.SP3, 1.29139, -0.175926, -1.9569));
		addAtom(new carbon(atom.SP3, 0.545136, 0.20329, 1.78273));
		addAtom(new carbon(atom.SP3, 0.376378, 1.71754, 1.79225));
		addAtom(new
			carbon(atom.SP3, -0.81981, -0.470738, 1.79274));
		addAtom(new
			carbon(atom.SP3, -0.835237, -0.437379, -0.692343));
		addAtom(new
			carbon(atom.SP3, -1.59629, -0.0263412, 0.561103));
		addAtom(new
			carbon(atom.SP3, -1.12353, 2.08724, -0.670977));
		addAtom(new carbon(atom.SP3, -1.10071, 2.08643, 1.79909));
		addAtom(new carbon(atom.SP3, -1.77095, 1.48861, 0.570283));
		addAtom(new carbon(atom.SP3, 2.51972, 1.94393, 3.01535));
		addAtom(new carbon(atom.SP3, 2.68997, 0.430124, 3.00373));
		addAtom(new carbon(atom.SP3, 1.31994, -0.233087, 3.01872));
		addAtom(new carbon(atom.SP3, 1.03575, 2.28124, 3.04187));
		addAtom(new carbon(atom.SP3, 3.16474, 2.5522, 1.77688));
		addAtom(new
			carbon(atom.SP3, 3.45219, 0.00815222, 1.75462));
		addAtom(new hydrogen(-1.19902, 3.1954, 1.7674));
		addAtom(new hydrogen(0.899848, 3.38521, 3.06999));
		addAtom(new hydrogen(-1.24954, 3.19306, -0.65931));
		addAtom(new hydrogen(0.905066, 3.42212, 0.565378));
		addAtom(new hydrogen(3.01947, 3.65579, 1.78769));
		addAtom(new hydrogen(0.886522, 3.45268, -1.91908));
		addAtom(new hydrogen(3.05414, 3.67335, -0.709896));
		addAtom(new hydrogen(2.96067, 2.43536, -2.86844));
		addAtom(new hydrogen(3.21483, 0.181941, -2.88331));
		addAtom(new hydrogen(4.24492, 2.3021, -0.72968));
		addAtom(new hydrogen(4.25275, 2.31935, 1.76957));
		addAtom(new hydrogen(4.45743, 0.485219, 1.75609));
		addAtom(new hydrogen(3.00582, 2.36113, 3.92512));
		addAtom(new hydrogen(3.25987, 0.116467, 3.90685));
		addAtom(new hydrogen(-1.37437, -0.172561, 2.71039));
		addAtom(new hydrogen(0.764023, 0.0764612, 3.93201));
		addAtom(new hydrogen(-1.58441, 1.69809, 2.72214));
		addAtom(new hydrogen(0.565584, 1.82266, 3.94069));
		addAtom(new hydrogen(4.4413, 0.543444, -0.73476));
		addAtom(new
			carbon(atom.SP3, 3.61692, -1.45838, -0.736331));
		addAtom(new carbon(atom.SP3, 1.48297, -1.72179, 0.521979));
		addAtom(new
			carbon(atom.SP3, -0.643049, -1.98501, 1.78011));
		addAtom(new carbon(atom.SP3, 3.6138, -1.50761, 1.74441));
		addAtom(new carbon(atom.SP3, 1.48955, -1.74672, 3.017));
		addAtom(new carbon(atom.SP3, 1.46833, -1.68795, -1.96639));
		addAtom(new
			carbon(atom.SP3, -0.659603, -1.95072, -0.707969));
		addAtom(new carbon(atom.SP3, 2.24355, -2.16844, 1.76378));
		addAtom(new
			carbon(atom.SP3, 0.117915, -2.39143, 0.524694));
		addAtom(new
			carbon(atom.SP3, 2.24794, -2.12574, -0.732645));
		addAtom(new hydrogen(2.35855, -3.27563, 1.75615));
		addAtom(new hydrogen(0.246415, -3.49682, 0.5085));
		addAtom(new hydrogen(2.37612, -3.23102, -0.749381));
		addAtom(new hydrogen(2.02564, -1.98716, -2.88204));
		addAtom(new hydrogen(2.06187, -2.05858, 3.91909));
		addAtom(new hydrogen(4.19435, -1.82764, 2.63811));
		addAtom(new hydrogen(4.14451, -1.82515, 0.819169));
		addAtom(new hydrogen(4.18238, -1.76447, 0.17217));
		addAtom(new hydrogen(4.17915, -1.76588, -1.64617));
		addAtom(new carbon(atom.SP3, 0.100929, -2.3571, -1.9632));
		addAtom(new
			carbon(atom.SP3, 0.504358, 0.262713, -3.18527));
		addAtom(new
			carbon(atom.SP3, -1.61942, -0.00143071, -1.92259));
		addAtom(new
			carbon(atom.SP3, -0.68302, -1.92243, -3.19444));
		addAtom(new
			carbon(atom.SP3, -0.861557, -0.409994, -3.17745));
		addAtom(new hydrogen(0.230162, -3.46235, -1.97663));
		addAtom(new
			carbon(atom.SP3, -2.96153, -0.699749, 0.568317));
		addAtom(new carbon(atom.SP3, -3.25529, 1.82946, 0.580452));
		addAtom(new
			carbon(atom.SP3, -3.92219, 1.24388, -0.655916));
		addAtom(new
			carbon(atom.SP3, -3.74247, -0.268683, -0.666072));
		addAtom(new carbon(atom.SP3, -3.723, -0.276649, 1.81773));
		addAtom(new carbon(atom.SP3, -3.89916, 1.23691, 1.82541));
		addAtom(new carbon(atom.SP3, -2.9825, -0.680134, -1.9187));
		addAtom(new carbon(atom.SP3, -1.79497, 1.51208, -1.90991));
		addAtom(new carbon(atom.SP3, 0.330736, 1.77577, -3.1719));
		addAtom(new carbon(atom.SP3, -1.15284, 2.11948, -3.14906));
		addAtom(new hydrogen(1.05716, -0.0360293, -4.10387));
		addAtom(new hydrogen(0.797239, 2.21203, -4.08329));
		addAtom(new carbon(atom.SP3, -3.2776, 1.85858, -1.88978));
		addAtom(new carbon(atom.SP3, -1.99993, -2.67318, 1.77153));
		addAtom(new
			carbon(atom.SP3, -2.02447, -2.62413, -0.701452));
		addAtom(new
			carbon(atom.SP3, -2.04589, -2.60206, -3.18661));
		addAtom(new
			carbon(atom.SP3, -2.80713, -2.19305, -1.93326));
		addAtom(new
			carbon(atom.SP3, -2.78654, -2.21436, 0.551655));
		addAtom(new hydrogen(-1.9091, -3.70656, -3.19362));
		addAtom(new hydrogen(-1.89126, -3.7291, -0.711017));
		addAtom(new hydrogen(-1.8476, -3.77515, 1.71876));
		addAtom(new hydrogen(-4.73937, -0.763194, -0.660411));
		addAtom(new hydrogen(-4.72318, -0.764126, 1.81956));
		addAtom(new hydrogen(-3.15444, -0.587996, 2.7223));
		addAtom(new hydrogen(-3.55417, -0.366873, -2.82035));
		addAtom(new hydrogen(-3.76681, 1.46516, -2.80829));
		addAtom(new hydrogen(-3.39602, 2.96491, -1.84839));
		addAtom(new hydrogen(-1.64425, 1.70936, -4.05945));
		addAtom(new hydrogen(-1.2782, 3.22528, -3.12552));
		addAtom(new hydrogen(-1.43976, -0.0904274, -4.07295));
		addAtom(new hydrogen(-2.62334, -2.29372, -4.08643));
		addAtom(new hydrogen(-0.126167, -2.21743, -4.11146));
		addAtom(new carbon(atom.SP3, 0.124798, -2.42103, 3.02197));
		addAtom(new hydrogen(-0.438217, -2.12005, 3.93371));
		addAtom(new hydrogen(0.254933, -3.52668, 3.0161));
		addAtom(new hydrogen(-2.55651, -2.41647, 2.70001));
		addAtom(new hydrogen(-3.78663, -2.70271, 0.55196));
		addAtom(new hydrogen(-3.80511, -2.68564, -1.92534));
		addAtom(new hydrogen(-4.98399, 1.48531, 1.8321));
		addAtom(new hydrogen(-3.41268, 1.66567, 2.72961));
		addAtom(new hydrogen(-3.38621, 2.93508, 0.575441));
		addAtom(new hydrogen(-5.00805, 1.4875, -0.643581));
		addBond(45, 63);
		addBond(62, 45);
		addBond(61, 48);
		addBond(60, 48);
		addBond(49, 59);
		addBond(50, 58);
		addBond(54, 57);
		addBond(53, 56);
		addBond(52, 55);
		addBond(46, 54);
		addBond(50, 54);
		addBond(45, 54);
		addBond(51, 53);
		addBond(47, 53);
		addBond(46, 53);
		addBond(52, 46);
		addBond(52, 49);
		addBond(48, 52);
		addBond(15, 51);
		addBond(11, 50);
		addBond(22, 49);
		addBond(25, 48);
		addBond(14, 47);
		addBond(9, 46);
		addBond(10, 45);
		addBond(44, 10);
		addBond(23, 43);
		addBond(18, 42);
		addBond(14, 40);
		addBond(22, 41);
		addBond(20, 38);
		addBond(21, 39);
		addBond(25, 37);
		addBond(3, 35);
		addBond(36, 24);
		addBond(2, 34);
		addBond(0, 33);
		addBond(3, 32);
		addBond(1, 31);
		addBond(24, 30);
		addBond(5, 29);
		addBond(17, 28);
		addBond(23, 27);
		addBond(18, 26);
		addBond(23, 20);
		addBond(13, 23);
		addBond(22, 12);
		addBond(21, 22);
		addBond(21, 25);
		addBond(20, 21);
		addBond(24, 20);
		addBond(4, 24);
		addBond(7, 25);
		addBond(18, 13);
		addBond(6, 17);
		addBond(8, 15);
		addBond(12, 14);
		addBond(16, 14);
		addBond(15, 16);
		addBond(19, 17);
		addBond(19, 18);
		addBond(19, 16);
		addBond(5, 13);
		addBond(9, 12);
		addBond(12, 13);
		addBond(11, 2);
		addBond(8, 11);
		addBond(9, 7);
		addBond(8, 9);
		addBond(10, 2);
		addBond(7, 10);
		addBond(6, 8);
		addBond(4, 7);
		addBond(4, 3);
		addBond(5, 4);
		addBond(6, 5);
		addBond(1, 6);
		addBond(0, 2);
		addBond(0, 3);
		addBond(0, 1);
		addBond(50, 64);
		addBond(64, 51);
		addBond(66, 15);
		addBond(65, 11);
		addBond(64, 67);
		addBond(67, 68);
		addBond(65, 68);
		addBond(68, 66);
		addBond(69, 64);
		addBond(70, 16);
		addBond(71, 19);
		addBond(70, 73);
		addBond(71, 72);
		addBond(72, 73);
		addBond(70, 74);
		addBond(71, 75);
		addBond(75, 74);
		addBond(73, 76);
		addBond(66, 76);
		addBond(77, 17);
		addBond(77, 66);
		addBond(78, 1);
		addBond(78, 79);
		addBond(79, 77);
		addBond(78, 65);
		addBond(81, 78);
		addBond(80, 65);
		addBond(82, 72);
		addBond(82, 77);
		addBond(85, 67);
		addBond(84, 51);
		addBond(83, 47);
		addBond(87, 83);
		addBond(87, 84);
		addBond(86, 84);
		addBond(86, 85);
		addBond(86, 76);
		addBond(87, 70);
		addBond(90, 83);
		addBond(89, 84);
		addBond(88, 85);
		addBond(91, 73);
		addBond(92, 74);
		addBond(93, 74);
		addBond(94, 76);
		addBond(96, 82);
		addBond(82, 95);
		addBond(97, 79);
		addBond(98, 79);
		addBond(99, 68);
		addBond(100, 85);
		addBond(101, 67);
		addBond(102, 47);
		addBond(102, 49);
		addBond(104, 102);
		addBond(103, 102);
		addBond(105, 83);
		addBond(106, 87);
		addBond(107, 86);
		addBond(109, 75);
		addBond(108, 75);
		addBond(110, 71);
		addBond(111, 72);
	}
}
