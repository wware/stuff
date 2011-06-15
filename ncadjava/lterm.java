/**
 * lterm.java - MM2-style length energy term
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

public class lterm extends term {
	public static final String rcsid =
	    "$Id: lterm.java,v 1.5 1999/01/13 04:18:01 wware Exp $";
	private double ks, r0;
	public lterm(atom a1, atom a2) {
		int i;
		boolean found;
		 myAtoms = new atom[2];
		 myAtoms[0] = a1;
		 myAtoms[1] = a2;
		for (i = 0, found = false;
		     i < lengthCoeffs.length && !found; i++)
			if ((a1.atomicNumber() == lengthCoeffs[i][0]
			     && a1.hybridization == lengthCoeffs[i][1]
			     && a2.atomicNumber() == lengthCoeffs[i][2]
			     && a2.hybridization == lengthCoeffs[i][3])
			    || (a1.atomicNumber() == lengthCoeffs[i][2]
				&& a1.hybridization == lengthCoeffs[i][3]
				&& a2.atomicNumber() == lengthCoeffs[i][0]
				&& a2.hybridization == lengthCoeffs[i][1])) {
				found = true;
				ks = lengthCoeffs[i][4];
				r0 = lengthCoeffs[i][5];
			}
		if (!found) {
			// something innocuous
			ks = 2.0;
			r0 = 1.2;
		}
	}
	protected void buildTerm(Vector v, Vector termList) {
		lterm t = new lterm((atom) v.elementAt(0),
				    (atom) v.elementAt(1));
		if (t.myAtoms[0].x[0] < t.myAtoms[1].x[0]) {
			termList.addElement(t);
		}
	}
	public int termLength() {
		return 2;
	}
	protected String repr2() {
		return " length " +
		    (new Double(ks)).toString() + " " +
		    (new Double(r0)).toString();
	}

	/* I've been hacking a bit with acceptable forms for the length-energy term.
	 * _Nanosystems_ lists one simple expression with a square and cubic term
	 * for close distances, and various kinds of exponential expressions for
	 * farther away. But these depend on numbers called De and beta and I don't
	 * have tables for those (just the few examples in _Nanosystems_). So after
	 * some puttering and graphing and algebra, I came up with the following.
	 * Energies are in attojoules (10^-18 joules), distances are in angstroms
	 * (10^-10 m), forces are in attojoules per angstrom (10^-8 N), and
	 * spring constants are in attojoules per angstrom-squared (100 N/m). This
	 * is consistent with the units in _Nanosystems_, and also the units used
	 * in the 'mm2.prm' parameter file I found on Jay Ponder's ftp site.
	 * Expressions for energy and force, where ks and r0 are a function of
	 * the two atoms involved:
	 *
	 *   dr = r - r0;
	 *
	 * Energy (r) :=
	 *   if (dr < rthresh)
	 *     { return 0.5 * ks * dr * dr * (1 - kc * dr) - z * ks; }
	 *   else
	 *     { return (-a * ks / beta) * exp (-beta * dr); }
	 *
	 * Force (r) :=
	 *   if (dr < rthresh)
	 *     { return -(ks * dr * (1 - 1.5 * kc * dr)); }
	 *   else
	 *     { return -a * ks * exp (-beta * dr); }
	 *
	 *
	 * A, beta, z, kcubic, and rthresh are defined below. Notice my use of
	 * 'beta' differs from the use in _Nanosystems_, but plays a similar role.
	 *
	 * These follow Drexler's quadratic-cubic form for dr < rthresh, and for
	 * dr > rthresh, they follow an exponential drop-off which is pretty
	 * continuous for both energy and force. The only point that I think would
	 * be worth quibbling over would be the value of beta, which I picked so
	 * that it would look "reasonable" on a graph.
	 */
	private static final double kcubic = 2.0;	/* (0.5 angstrom)^-1 */
	private static final double rthresh = 1 / (3 * kcubic);
	private static final double beta = 3.0;
	private static final double a =
	    rthresh * (1 -
		       1.5 * kcubic * rthresh) * Math.exp(beta * rthresh);
	private static final double z =
	    (a / beta) * Math.exp(-beta * rthresh) +
	    0.5 * rthresh * rthresh * (1 - kcubic * rthresh);
	public void computeForces() {
		int i;

		// compute forces on each atom, add it to the atom's force vector
		double[] diff = new double[3];
		double r, m;
		for (i = 0, r = 0.0; i < 3; i++) {
			diff[i] = myAtoms[0].x[i] - myAtoms[1].x[i];
			r += diff[i] * diff[i];
		}
		r = Math.sqrt(r);
		double rdiff = (r - r0), expr;
		if (rdiff < rthresh)
			m = ks * rdiff * (1 - 1.5 * kcubic * rdiff);
		else
			m = a * ks * Math.exp(-beta * rdiff);
		// at this point, m is du/dr
		m /= r;
		// m > 0 attract, m < 0 repel
		for (i = 0; i < 3; i++) {
			myAtoms[0].f[i] -= m * diff[i];
			myAtoms[1].f[i] += m * diff[i];
		}
	}

	// Coefficient data
	// Forces are in aJ per square angstrom (spring constants)
	private final static double[][] lengthCoeffs = {
		{C, atom.SP3, C, atom.SP3, 4.400, 1.523},
		{C, atom.SP3, C, atom.SP2, 4.400, 1.497},
		{C, atom.SP3, C, atom.SP, 5.200, 1.470},
		{C, atom.SP3, H, atom.NONE, 4.600, 1.113},
		{C, atom.SP3, O, atom.SP3, 5.360, 1.402},
		{C, atom.SP3, N, atom.SP3, 5.100, 1.438},
		{C, atom.SP3, N, atom.SP2, 3.520, 1.437},
		{C, atom.SP3, N, atom.SP3, 5.100, 1.499},
		{C, atom.SP3, O, atom.SP2, 5.360, 1.414},
		{C, atom.SP2, C, atom.SP2, 9.600, 1.337},
		{C, atom.SP2, C, atom.SP, 9.900, 1.313},
		{C, atom.SP2, H, atom.NONE, 4.600, 1.101},
		{C, atom.SP2, O, atom.SP3, 6.000, 1.355},
		{C, atom.SP2, N, atom.SP3, 6.320, 1.377},
		{C, atom.SP2, N, atom.SP2, 5.000, 1.410},
		{C, atom.SP2, O, atom.SP2, 10.000, 1.225},
		{C, atom.SP, C, atom.SP, 15.600, 1.212},
		{C, atom.SP, H, atom.NONE, 5.900, 1.090},
		{C, atom.SP, N, atom.SP, 17.730, 1.158},
		{O, atom.SP3, O, atom.SP3, 3.950, 1.428},
		{N, atom.SP3, N, atom.SP3, 5.600, 1.381},
		{N, atom.SP3, H, atom.NONE, 6.100, 1.045}
	};
}
