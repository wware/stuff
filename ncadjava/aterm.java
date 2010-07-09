/**
 * aterm.java - MM2-style angle energy term
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

import java.lang.Math;
import java.util.Vector;

public class aterm extends term {
	public static final String rcsid =
	    "$Id: aterm.java,v 1.7 1999/01/13 05:27:47 wware Exp $";
	private static final double convert = 3.1415926 / 180;	// degrees to radians
	private double kth, th0;
	public aterm(atom a1, atom a2, atom a3) {
		int i;
		boolean found;
		 myAtoms = new atom[3];
		 myAtoms[0] = a1;
		 myAtoms[1] = a2;
		 myAtoms[2] = a3;
		for (i = 0, found = false;
		     i < angleCoeffs.length && !found; i++)
			if ((a1.atomicNumber() == angleCoeffs[i][0]
			     && a1.hybridization == angleCoeffs[i][1]
			     && a2.atomicNumber() == angleCoeffs[i][2]
			     && a2.hybridization == angleCoeffs[i][3]
			     && a3.atomicNumber() == angleCoeffs[i][4]
			     && a3.hybridization == angleCoeffs[i][5])
			    || (a1.atomicNumber() == angleCoeffs[i][4]
				&& a1.hybridization == angleCoeffs[i][5]
				&& a2.atomicNumber() == angleCoeffs[i][2]
				&& a2.hybridization == angleCoeffs[i][3]
				&& a3.atomicNumber() == angleCoeffs[i][0]
				&& a3.hybridization == angleCoeffs[i][1])) {
				found = true;
				kth = angleCoeffs[i][6];
				th0 = angleCoeffs[i][7] * convert;
			}
		if (!found) {
			kth = 0.3;
			th0 = 120.0 * convert;
		}
	}
	protected void buildTerm(Vector v, Vector termList) {
		aterm t = new aterm((atom) v.elementAt(0),
				    (atom) v.elementAt(1),
				    (atom) v.elementAt(2));
		if (t.myAtoms[0].x[0] < t.myAtoms[2].x[0]) {
			termList.addElement(t);
		}
	}
	public int termLength() {
		return 3;
	}
	protected String repr2() {
		return " angle " +
		    (new Double(kth)).toString() + " " +
		    (new Double(th0)).toString();
	}
	public void computeForces() {
		if (kth == 0.0)
			return;
		int i;
		// compute forces on each atom, add it to the atom's force vector
		double[] adiff = new double[3];
		double[] bdiff = new double[3];
		double aa = 0.0, ab = 0.0, bb = 0.0, th, tdif, duDth;
		for (i = 0; i < 3; i++) {
			adiff[i] = myAtoms[0].x[i] - myAtoms[1].x[i];
			bdiff[i] = myAtoms[2].x[i] - myAtoms[1].x[i];
			aa += adiff[i] * adiff[i];
			ab += adiff[i] * bdiff[i];
			bb += bdiff[i] * bdiff[i];
		}
		if (aa > 3.0 || bb > 3.0)
			return;
		th = acos(ab / sqrt(aa * bb));
		tdif = th - th0;
		duDth = kth * (tdif * (1.0 + 1.508 * tdif * tdif));
		double[] f0 = new double[3];
		double[] f2 = new double[3];
		double ff0 = 0.0, ff2 = 0.0;
		for (i = 0; i < 3; i++) {
			f0[i] = aa * bdiff[i] - ab * adiff[i];
			ff0 += f0[i] * f0[i];
			f2[i] = bb * adiff[i] - ab * adiff[i];
			ff2 += f2[i] * f2[i];
		}
		double m0 = duDth / sqrt(ff0 * aa);
		double m2 = duDth / sqrt(ff2 * bb);
		for (i = 0; i < 3; i++) {
			f0[i] *= m0;
			f2[i] *= m2;
			myAtoms[0].f[i] += f0[i];
			myAtoms[1].f[i] -= f0[i] + f2[i];
			myAtoms[2].f[i] += f2[i];
		}
	}
	private final static double[][] angleCoeffs = {
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, 0.450, 109.470},
		{C, atom.SP3, C, atom.SP3, C, atom.SP2, 0.450, 109.470},
		{C, atom.SP3, C, atom.SP3, C, atom.SP, 0.450, 109.470},
		{C, atom.SP3, C, atom.SP3, H, atom.NONE, 0.360, 109.390},
		{C, atom.SP3, C, atom.SP3, O, atom.SP3, 0.700, 107.500},
		{C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.570, 109.470},
		{C, atom.SP3, C, atom.SP3, N, atom.SP2, 0.500, 109.280},
		{C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.570, 103.500},
		{C, atom.SP3, C, atom.SP3, O, atom.SP2, 0.700, 107.500},
		{C, atom.SP2, C, atom.SP3, C, atom.SP2, 0.450, 109.470},
		{C, atom.SP2, C, atom.SP3, C, atom.SP, 0.470, 109.470},
		{C, atom.SP2, C, atom.SP3, H, atom.NONE, 0.360, 109.390},
		{C, atom.SP2, C, atom.SP3, O, atom.SP3, 0.700, 109.500},
		{C, atom.SP2, C, atom.SP3, N, atom.SP3, 1.045, 110.740},
		{C, atom.SP2, C, atom.SP3, N, atom.SP2, 0.500, 109.800},
		{C, atom.SP2, C, atom.SP3, N, atom.SP3, 1.045, 110.740},
		{C, atom.SP, C, atom.SP3, C, atom.SP, 0.470, 109.470},
		{C, atom.SP, C, atom.SP3, H, atom.NONE, 0.360, 109.390},
		{H, atom.NONE, C, atom.SP3, H, atom.NONE, 0.320, 109.400},
		{H, atom.NONE, C, atom.SP3, O, atom.SP3, 0.540, 106.700},
		{H, atom.NONE, C, atom.SP3, N, atom.SP3, 0.500, 108.800},
		{H, atom.NONE, C, atom.SP3, N, atom.SP2, 0.420, 109.000},
		{H, atom.NONE, C, atom.SP3, N, atom.SP3, 0.500, 108.800},
		{H, atom.NONE, C, atom.SP3, O, atom.SP2, 0.540, 106.700},
		{O, atom.SP3, C, atom.SP3, O, atom.SP3, 0.460, 99.900},
		{N, atom.SP3, C, atom.SP3, N, atom.SP3, 1.045, 110.740},
		{N, atom.SP3, C, atom.SP3, N, atom.SP3, 1.045, 110.740},
		{C, atom.SP3, C, atom.SP2, C, atom.SP3, 0.450, 117.200},
		{C, atom.SP3, C, atom.SP2, C, atom.SP2, 0.550, 121.400},
		{C, atom.SP3, C, atom.SP2, C, atom.SP, 0.470, 122.000},
		{C, atom.SP3, C, atom.SP2, H, atom.NONE, 0.360, 118.200},
		{C, atom.SP3, C, atom.SP2, O, atom.SP3, 0.500, 120.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, 0.430, 120.000},
		{C, atom.SP2, C, atom.SP2, H, atom.NONE, 0.360, 120.000},
		{C, atom.SP2, C, atom.SP2, O, atom.SP3, 0.700, 124.300},
		{C, atom.SP2, C, atom.SP2, N, atom.SP3, 0.616, 123.000},
		{C, atom.SP2, C, atom.SP2, N, atom.SP2, 0.500, 118.000},
		{C, atom.SP2, C, atom.SP2, O, atom.SP2, 0.600, 120.000},
		{C, atom.SP, C, atom.SP2, H, atom.NONE, 0.360, 121.100},
		{H, atom.NONE, C, atom.SP2, H, atom.NONE, 0.320, 119.000},
		{H, atom.NONE, C, atom.SP2, O, atom.SP3, 0.540, 116.400},
		{H, atom.NONE, C, atom.SP2, N, atom.SP3, 0.540, 119.000},
		{H, atom.NONE, C, atom.SP2, N, atom.SP2, 0.300, 109.000},
		{H, atom.NONE, C, atom.SP2, O, atom.SP2, 0.450, 108.000},
		{N, atom.SP2, C, atom.SP2, N, atom.SP2, 0.400, 120.000},
		{C, atom.SP3, C, atom.SP, C, atom.SP, 0.200, 180.000},
		{C, atom.SP3, C, atom.SP, N, atom.SP, 0.325, 180.000},
		{C, atom.SP2, C, atom.SP, C, atom.SP2, 0.400, 180.000},
		{C, atom.SP2, C, atom.SP, C, atom.SP, 0.470, 180.000},
		{C, atom.SP, C, atom.SP, H, atom.NONE, 0.360, 180.000},
		{C, atom.SP, C, atom.SP, O, atom.SP3, 0.360, 180.000},
		{C, atom.SP, C, atom.SP, O, atom.SP2, 0.360, 180.000},
		{C, atom.SP, C, atom.SP, N, atom.SP3, 0.360, 180.000},
		{C, atom.SP, C, atom.SP, N, atom.SP2, 0.360, 180.000},
		{C, atom.SP, C, atom.SP, N, atom.SP, 0.360, 180.000},
		{C, atom.SP3, O, atom.SP3, C, atom.SP3, 0.770, 106.800},
		{C, atom.SP3, O, atom.SP3, C, atom.SP2, 0.770, 110.800},
		{C, atom.SP3, O, atom.SP3, O, atom.SP3, 0.635, 98.700},
		{C, atom.SP3, N, atom.SP3, C, atom.SP3, 0.630, 107.700},
		{C, atom.SP3, N, atom.SP3, C, atom.SP2, 0.698, 107.000},
		{C, atom.SP3, N, atom.SP3, N, atom.SP3, 0.740, 105.500},
		{C, atom.SP3, N, atom.SP2, C, atom.SP3, 0.760, 126.000},
		{C, atom.SP3, N, atom.SP2, C, atom.SP2, 0.630, 119.900},
		{C, atom.SP2, N, atom.SP2, C, atom.SP2, 0.400, 107.000},
		{C, atom.SP3, N, atom.SP3, C, atom.SP3, 0.630, 108.600},
		{C, atom.SP3, N, atom.SP3, H, atom.NONE, 0.500, 109.470},
		{H, atom.NONE, N, atom.SP3, H, atom.NONE, 0.500, 104.500},
		{C, atom.SP3, O, atom.SP2, C, atom.SP2, 0.770, 113.600},
		{C, atom.SP2, O, atom.SP2, C, atom.SP2, 0.870, 113.950}
	};
}
