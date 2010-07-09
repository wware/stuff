/**
 * lrterm.java - MM2-style long-range (electrostatic and vdw) energy term
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

import java.lang.Math;
import java.util.Vector;

public class lrterm extends term {
	public static final String rcsid =
	    "$Id: lrterm.java,v 1.6 1999/01/13 04:36:02 wware Exp $";

	private int[][] exclusions;
	Vector atomList;

	private final static double dontKnowCorrectUnits = 1.0;

	private void hackCharges() {
		int i, j, k, n;
		boolean found;
		atom a1, a2;

		 n = atomList.size();
		for (j = 0; j < n; j++) {
			a1 = (atom) atomList.elementAt(j);
			for (k = 0; k < n; k++) {
				a2 = (atom) atomList.elementAt(k);
				for (i = 0, found = false;
				     i < dipoleMoments.length && !found;
				     i++) {
					if (a1.atomicNumber() ==
					    dipoleMoments[i][0]
					    && a1.hybridization ==
					    dipoleMoments[i][1]
					    && a2.atomicNumber() ==
					    dipoleMoments[i][2]
					    && a2.hybridization ==
					    dipoleMoments[i][3]) {
						double diffCharge =
						    dontKnowCorrectUnits *
						    dipoleMoments[i][4];
						 found = true;
						 a1.fractionalCharge
						    += diffCharge;
						 a2.fractionalCharge
						    -= diffCharge;
					} else if (a1.atomicNumber() ==
						   dipoleMoments[i][2]
						   && a1.hybridization ==
						   dipoleMoments[i][3]
						   && a2.
						   atomicNumber() ==
						   dipoleMoments[i][0]
						   && a2.hybridization ==
						   dipoleMoments[i][1]) {
						double diffCharge =
						    dontKnowCorrectUnits
						    * dipoleMoments[i][4];
						found = true;
						a1.fractionalCharge -=
						    diffCharge;
						a2.fractionalCharge +=
						    diffCharge;
					}
				}
			}
		}
	}

	private boolean bondChain(atom a1, atom a2, int depth) {
		int i;
		if (a1 == a2)
			return true;
		if (depth == 0)
			return false;
		if (a1.bonds == null)
			return false;
		if (depth == 1)
			return a1.bondWith(a2) != null;
		for (i = 0; i < a1.bonds.size(); i++)
			if (bondChain
			    (((bond) a1.bonds.elementAt(i)).otherAtom(a1),
			     a2, depth - 1))
				return true;
		return false;
	}

	public int termLength() {
		return 2;
	}
	public void buildTerm(Vector v, Vector termList) {
	}
	private final static int maxNumChained = 50;
	public void enumerate(Vector aList, Vector termList) {
		int i, j, n;

		atomList = aList;
		hackCharges();
		n = atomList.size();
		exclusions = new int[n][maxNumChained];
		for (i = 0; i < n; i++) {
			int numChained = 0;
			atom a1 = (atom) atomList.elementAt(i);
			for (j = i + 1; j < n; j++) {
				atom a2 = (atom) atomList.elementAt(j);
				if (bondChain(a1, a2, 2))
					exclusions[i][numChained++] = j;
			}
			/* trim each subarray to the smallest possible size */
			int[] temp = new int[numChained];
			for (j = 0; j < numChained; j++)
				temp[j] = exclusions[i][j];
			exclusions[i] = temp;
		}
		termList.addElement(this);
	}

	public void computeForces() {
		int i, j, k, n;
		n = atomList.size();
		for (i = 0; i < n; i++) {
			atom a1 = (atom) atomList.elementAt(i);
			k = i + 1;
			for (j = 0; j < exclusions[i].length; j++) {
				for (; k < exclusions[i][j]; k++) {
					atom a2 =
					    (atom) atomList.elementAt(k);
					computeForces(a1, a2);
				}
				k++;
			}
			for (; k < n; k++) {
				atom a2 = (atom) atomList.elementAt(k);
				computeForces(a1, a2);
			}
		}
	}

	/* units here are (maJ * nm) / (e * e), where e is charge on a proton */
	private final static double electricConstant =
	    // (8.9876 * 0.160206 * 0.160206 * 1000.0);
	    -1.0e-3;

	// should be negative, -1.0 is too big, -1.0e-12 is too small
	// -1e-6 too small, -1.0e-3 is in the ballpark

	private void computeForces(atom a1, atom a2) {
		int i;
		double rvdw = a1.vdwRadius() + a2.vdwRadius();
		double evdw = (a1.vdwEnergy() + a2.vdwEnergy()) / 2;
		// let's ignore integer charge for the time being
		double q1q2 = a1.fractionalCharge * a2.fractionalCharge;
		double[] diff = new double[3];
		double m, r, r2, r_1, r_2, r_6;
		for (i = 0, r2 = 0.0; i < 3; i++) {
			diff[i] = a1.x[i] - a2.x[i];
			r2 += diff[i] * diff[i];
		}
		r = Math.sqrt(r2);
		// m = electricConstant * q1q2 / (r2 * r);

		r_1 = rvdw / r;
		r_2 = r_1 * r_1;
		r_6 = r_2 * r_2 * r_2;
		// m -= 0.012 * evdw * r_1 * r_6 * (r_6 - 1.0);
		m = -0.012 * evdw * r_1 * r_6 * (r_6 - 1.0);

		// m > 0 attract, m < 0 repel
		for (i = 0; i < 3; i++) {
			a1.f[i] -= m * diff[i];
			a2.f[i] += m * diff[i];
		}
	}

	// These are covalent bond dipole moments, due to
	// differences in electronegativity. Actually, these
	// are dipoleMoments divided by bondLengths, which
	// gives fractionalCharges
	private final static double[][] dipoleMoments = {
		{C, atom.SP3, C, atom.SP2, 0.300 / 1.497},
		{C, atom.SP3, C, atom.SP, 0.750 / 1.470},
		{C, atom.SP3, O, atom.SP3, 0.440 / 1.402},
		{C, atom.SP3, N, atom.SP3, 0.040 / 1.438},
		{C, atom.SP3, N, atom.SP2, 1.470 / 1.437},
		{C, atom.SP3, C, atom.SP3, 0.150 / 1.502},
		{C, atom.SP3, N, atom.SP2, 1.260 / 1.470},
		{C, atom.SP3, C, atom.SP2, 0.300 / 1.497},
		{C, atom.SP3, O, atom.SP2, 0.220 / 1.414},
		{C, atom.SP3, N, atom.SP2, 1.260 / 1.488},
		{C, atom.SP3, N, atom.SP2, 1.350 / 1.498},
		{C, atom.SP3, C, atom.SP2, 0.300 / 1.497},
		{C, atom.SP3, O, atom.SP3, 1.900 / 1.480},
		{C, atom.SP3, C, atom.SP3, 0.300 / 1.509},
		{C, atom.SP3, N, atom.SP2, 1.260 / 1.470},
		{C, atom.SP2, O, atom.SP3, 0.001 / 1.355},
		{C, atom.SP2, N, atom.SP3, -0.400 / 1.377},
		{C, atom.SP2, N, atom.SP2, 1.300 / 1.410},
		{C, atom.SP2, C, atom.SP3, -0.150 / 1.467},
		{C, atom.SP2, N, atom.SP2, 0.583 / 1.260},
		{C, atom.SP2, N, atom.SP2, 0.870 / 1.266},
		{C, atom.SP2, O, atom.SP2, 0.950 / 1.225},
		{C, atom.SP2, N, atom.SP2, 1.700 / 1.463},
		{C, atom.SP2, N, atom.SP2, 0.583 / 1.260},
		{C, atom.SP, N, atom.SP, 3.400 / 1.158},
		{O, atom.SP3, H, atom.NONE, -1.115 / 0.942},
		{O, atom.SP3, H, atom.NONE, -0.700 / 0.972},
		{O, atom.SP3, H, atom.NONE, -0.700 / 0.972},
		{O, atom.SP2, N, atom.SP2, -2.600 / 1.268},
		{O, atom.SP2, N, atom.SP2, -2.530 / 1.220},
		{N, atom.SP3, H, atom.NONE, -0.760 / 1.020},
		{N, atom.SP2, H, atom.NONE, -1.310 / 1.022},
		{C, atom.SP3, N, atom.SP2, 1.700 / 1.477},
		{H, atom.NONE, N, atom.SP2, 0.600 / 1.022},
		{H, atom.NONE, N, atom.SP2, 0.600 / 1.022},
		{H, atom.NONE, O, atom.SP2, 0.700 / 0.972},
		{N, atom.SP2, N, atom.SP2, 0.300 / 1.230},
		{O, atom.SP3, C, atom.SP3, -2.800 / 1.236}
	};
}
