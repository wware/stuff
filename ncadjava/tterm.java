/**
 * tterm.java - MM2-style torsion energy term
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

public class tterm extends term {
	public static final String rcsid =
	    "$Id: tterm.java,v 1.6 1999/01/13 04:18:57 wware Exp $";
	private double v1, v2, v3;
	public tterm(atom a1, atom a2, atom a3, atom a4) {
		int i;
		boolean found;
		 myAtoms = new atom[4];
		 myAtoms[0] = a1;
		 myAtoms[1] = a2;
		 myAtoms[2] = a3;
		 myAtoms[3] = a4;
		for (i = 0, found = false;
		     i < torsionCoeffs.length && !found; i++)
			if ((a1.atomicNumber() == torsionCoeffs[i][0]
			     && a1.hybridization == torsionCoeffs[i][1]
			     && a2.atomicNumber() == torsionCoeffs[i][2]
			     && a2.hybridization == torsionCoeffs[i][3]
			     && a3.atomicNumber() == torsionCoeffs[i][4]
			     && a3.hybridization == torsionCoeffs[i][5]
			     && a4.atomicNumber() == torsionCoeffs[i][6]
			     && a4.hybridization == torsionCoeffs[i][7])
			    || (a1.atomicNumber() == torsionCoeffs[i][6]
				&& a1.hybridization == torsionCoeffs[i][7]
				&& a2.atomicNumber() == torsionCoeffs[i][4]
				&& a2.hybridization == torsionCoeffs[i][5]
				&& a3.atomicNumber() == torsionCoeffs[i][2]
				&& a3.hybridization == torsionCoeffs[i][3]
				&& a4.atomicNumber() == torsionCoeffs[i][0]
				&& a4.hybridization ==
				torsionCoeffs[i][1])) {
				found = true;
				// convert table numbers from maJ to aJ (10^-18 joules)
				v1 = 0.001 * torsionCoeffs[i][8];
				v2 = 0.001 * torsionCoeffs[i][9];
				v3 = 0.001 * torsionCoeffs[i][10];
			}
		if (!found)
			 v1 = v2 = v3 = 0.0;
	}

	protected void buildTerm(Vector v, Vector termList) {
		tterm t = new tterm((atom) v.elementAt(0),
				    (atom) v.elementAt(1),
				    (atom) v.elementAt(2),
				    (atom) v.elementAt(3));
		if (v1 == 0.0 && v2 == 0.0 && v3 == 0.0)
			return;
		if (t.myAtoms[0].x[0] < t.myAtoms[3].x[0]) {
			termList.addElement(t);
		}
	}
	public int termLength() {
		return 4;
	}
	protected String repr2() {
		return " torsion " +
		    (new Double(v1)).toString() + " " +
		    (new Double(v2)).toString() + " " +
		    (new Double(v3)).toString();
	}

	private final static double PI = 3.14159265258979;
	// I found, much to my dismay, that I had been miscalculating torsion
	// forces for years! How embarassing. Now I've decided to just steal the
	// correct torsion code from the NAMD program and translate it from C++
	// to Java.
	public void computeForces() {
		double r12[], r23[], r34[];	//  Vectors between atoms
		double f1[], f2[], f3[];	//  Forces 1 through 3
		double A[], B[], C[];	//  Cross product vectors
		double dcosdA[];	//  Derivative d(cos(phi))/dA
		double dcosdB[];	//  Derivative d(cos(phi))/dB
		double dsindC[];	//  Derivative d(sin(phi))/dC
		double dsindB[];	//  Derivative d(sin(phi))/dB
		double rA, rB, rC;	//  Length of vectors A, B, and C
		double phi;	//  angle between the plans
		double cos_phi;	//  cos(phi)
		double sin_phi;	//  sin(phi)
		double delta;	//  Phase shift of the current dihedral
		double k;	//  Force constant of the current dihedral
		int n;		//  Periodicity
		double K, K1;	//  Calculated factor
		double diff;	//  Difference between phi and phi0
		int mult_num;	//  Current multiple we are calculating

		if (v1 == 0.0 && v2 == 0.0 && v3 == 0.0)
			return;

		r12 = new double[3];
		r23 = new double[3];
		r34 = new double[3];
		f1 = new double[3];
		f2 = new double[3];
		f3 = new double[3];
		A = new double[3];
		B = new double[3];
		C = new double[3];
		dcosdA = new double[3];
		dcosdB = new double[3];
		dsindC = new double[3];
		dsindB = new double[3];

		// Calculate the vectors between atoms
		r12[0] = myAtoms[0].x[0] - myAtoms[1].x[0];
		r12[1] = myAtoms[0].x[1] - myAtoms[1].x[1];
		r12[2] = myAtoms[0].x[2] - myAtoms[1].x[2];

		r23[0] = myAtoms[1].x[0] - myAtoms[2].x[0];
		r23[1] = myAtoms[1].x[1] - myAtoms[2].x[1];
		r23[2] = myAtoms[1].x[2] - myAtoms[2].x[2];

		r34[0] = myAtoms[2].x[0] - myAtoms[3].x[0];
		r34[1] = myAtoms[2].x[1] - myAtoms[3].x[1];
		r34[2] = myAtoms[2].x[2] - myAtoms[3].x[2];

		//  Calculate the cross products
		A[0] = r12[1] * r23[2] - r23[1] * r12[2];
		A[1] = -r12[0] * r23[2] + r23[0] * r12[2];
		A[2] = r12[0] * r23[1] - r23[0] * r12[1];

		B[0] = r23[1] * r34[2] - r34[1] * r23[2];
		B[1] = -r23[0] * r34[2] + r34[0] * r23[2];
		B[2] = r23[0] * r34[1] - r34[0] * r23[1];

		C[0] = r23[1] * A[2] - A[1] * r23[2];
		C[1] = -r23[0] * A[2] + A[0] * r23[2];
		C[2] = r23[0] * A[1] - A[0] * r23[1];

		//  Calculate the distances
		rA = Math.sqrt(A[0] * A[0] + A[1] * A[1] + A[2] * A[2]);
		rB = Math.sqrt(B[0] * B[0] + B[1] * B[1] + B[2] * B[2]);
		rC = Math.sqrt(C[0] * C[0] + C[1] * C[1] + C[2] * C[2]);

		//  Calculate the sin and cos
		//  cos = A*B/(rA*rB)
		//  sin = C*B/(rC*rB)
		cos_phi =
		    (A[0] * B[0] + A[1] * B[1] + A[2] * B[2]) / (rA * rB);
		sin_phi =
		    (C[0] * B[0] + C[1] * B[1] + C[2] * B[2]) / (rC * rB);

		//  Normalize B
		rB = 1 / rB;

		B[0] *= rB;
		B[1] *= rB;
		B[2] *= rB;

		//  Get phi, assign the sign based on the sine value

		//  Make sure that the cosine value is acceptable.  With roundoff, you
		//  can get values like 1.0+2e-16, which makes acos puke.  So instead,
		//  just set these kinds of values to exactly 1.0
		//                if (cos_phi>1.0)
		//                        cos_phi = 1.0;
		//                else if (cos_phi < -1.0)
		//                        cos_phi = -1.0;

		//                phi = acos(cos_phi);
		//                CHECK_DOMAIN();
		//                phi = -copysign(phi, sin_phi);

		// I think atan2 will do all the above stuff accurately
		// RKB
		phi = -Math.atan2(sin_phi, cos_phi);
		// CHECK_DOMAIN(); ??????

		// if (fabs(sin_phi) > 0.1)
		if (sin_phi > 0.1 || sin_phi < -0.1) {
			//  Normalize A
			rA = 1 / rA;

			A[0] *= rA;
			A[1] *= rA;
			A[2] *= rA;

			dcosdA[0] = -rA * (B[0] - cos_phi * A[0]);
			dcosdA[1] = -rA * (B[1] - cos_phi * A[1]);
			dcosdA[2] = -rA * (B[2] - cos_phi * A[2]);

			dcosdB[0] = -rB * (A[0] - cos_phi * B[0]);
			dcosdB[1] = -rB * (A[1] - cos_phi * B[1]);
			dcosdB[2] = -rB * (A[2] - cos_phi * B[2]);
		} else {
			//  Normalize C
			rC = 1 / rC;

			C[0] *= rC;
			C[1] *= rC;
			C[2] *= rC;

			dsindC[0] = -rC * (B[0] - sin_phi * C[0]);
			dsindC[1] = -rC * (B[1] - sin_phi * C[1]);
			dsindC[2] = -rC * (B[2] - sin_phi * C[2]);

			dsindB[0] = -rB * (C[0] - sin_phi * B[0]);
			dsindB[1] = -rB * (C[1] - sin_phi * B[1]);
			dsindB[2] = -rB * (C[2] - sin_phi * B[2]);
		}

		//  Loop through the multiple parameter sets for this
		//  bond.  We will only loop more than once if this
		//  has multiple parameter sets from Charmm22
		for (mult_num = 0; mult_num < 3; mult_num++) {
			switch (mult_num) {
			case 0:
				n = 1;
				k = v1;
				break;
			case 1:
				n = 2;
				k = -v2;
				break;
			default:
				n = 3;
				k = v3;
				break;
			}
			delta = 0;

			if (k != 0.0) {
				//  Calculate the energy
				K = k * (1 + Math.cos(n * phi + delta));
				K1 = -n * k * Math.sin(n * phi + delta);

				//  Next, we want to calculate the forces.  In order
				//  to do that, we first need to figure out whether the
				//  sin or cos form will be more stable.  For this,
				//  just look at the value of phi
				// if (fabs(sin_phi) > 0.1)
				if (sin_phi > 0.1 || sin_phi < -0.1) {
					//  use the sin version to avoid 1/cos terms
					K1 = K1 / sin_phi;

					f1[0] = K1 * (r23[1] *
						      dcosdA[2] -
						      r23[2] * dcosdA[1]);
					f1[1] = K1 * (r23[2] *
						      dcosdA[0] -
						      r23[0] * dcosdA[2]);
					f1[2] = K1 * (r23[0] *
						      dcosdA[1] -
						      r23[1] * dcosdA[0]);

					f3[0] = K1 * (r23[2] *
						      dcosdB[1] -
						      r23[1] * dcosdB[2]);
					f3[1] = K1 * (r23[0] *
						      dcosdB[2] -
						      r23[2] * dcosdB[0]);
					f3[2] = K1 * (r23[1] *
						      dcosdB[0] -
						      r23[0] * dcosdB[1]);

					f2[0] = K1 * (r12[2] *
						      dcosdA[1] -
						      r12[1] *
						      dcosdA[2] +
						      r34[1] *
						      dcosdB[2] -
						      r34[2] * dcosdB[1]);
					f2[1] = K1 * (r12[0] *
						      dcosdA[2] -
						      r12[2] *
						      dcosdA[0] +
						      r34[2] *
						      dcosdB[0] -
						      r34[0] * dcosdB[2]);
					f2[2] = K1 * (r12[1] *
						      dcosdA[0] -
						      r12[0] *
						      dcosdA[1] +
						      r34[0] *
						      dcosdB[1] -
						      r34[1] * dcosdB[0]);
				} else {
					//  This angle is closer to 0 or 180 than it is to 
					//  90, so use the cos version to avoid 1/sin terms
					K1 = -K1 / cos_phi;

					f1[0] = K1 *
					    ((r23[1] * r23[1] +
					      r23[2] * r23[2]) *
					     dsindC[0] -
					     r23[0] * r23[1] *
					     dsindC[1] -
					     r23[0] * r23[2] * dsindC[2]);
					f1[1] = K1 *
					    ((r23[2] * r23[2] +
					      r23[0] * r23[0]) *
					     dsindC[1] -
					     r23[1] * r23[2] *
					     dsindC[2] -
					     r23[1] * r23[0] * dsindC[0]);
					f1[2] = K1 *
					    ((r23[0] * r23[0] +
					      r23[1] * r23[1]) *
					     dsindC[2] -
					     r23[2] * r23[0] *
					     dsindC[0] -
					     r23[2] * r23[1] * dsindC[1]);

					f3[0] = K1 * (dsindB[1] *
						      r23[2] -
						      r23[1] * dsindB[2]);
					f3[1] = K1 * (dsindB[2] *
						      r23[0] -
						      r23[2] * dsindB[0]);
					f3[2] = K1 * (dsindB[0] *
						      r23[1] -
						      r23[0] * dsindB[1]);

					f2[0] = K1 *
					    (-
					     (r23[1] * r12[1] +
					      r23[2] * r12[2]) *
					     dsindC[0] +
					     (2.0 * r23[0] *
					      r12[1] -
					      r12[0] * r23[1]) *
					     dsindC[1] +
					     (2.0 * r23[0] *
					      r12[2] -
					      r12[0] * r23[2]) *
					     dsindC[2] +
					     dsindB[2] * r34[1] -
					     dsindB[1] * r34[2]);
					f2[1] = K1 *
					    (-
					     (r23[2] * r12[2] +
					      r23[0] * r12[0]) *
					     dsindC[1] +
					     (2.0 * r23[1] *
					      r12[2] -
					      r12[1] * r23[2]) *
					     dsindC[2] +
					     (2.0 * r23[1] *
					      r12[0] -
					      r12[1] * r23[0]) *
					     dsindC[0] +
					     dsindB[0] * r34[2] -
					     dsindB[2] * r34[0]);
					f2[2] = K1 *
					    (-
					     (r23[0] * r12[0] +
					      r23[1] * r12[1]) *
					     dsindC[2] +
					     (2.0 * r23[2] *
					      r12[0] -
					      r12[2] * r23[0]) *
					     dsindC[0] +
					     (2.0 * r23[2] *
					      r12[1] -
					      r12[2] * r23[1]) *
					     dsindC[1] +
					     dsindB[1] * r34[0] -
					     dsindB[0] * r34[1]);
				}

				myAtoms[0].f[0] += f1[0];
				myAtoms[0].f[1] += f1[1];
				myAtoms[0].f[2] += f1[2];

				myAtoms[1].f[0] += f2[0] - f1[0];
				myAtoms[1].f[1] += f2[1] - f1[1];
				myAtoms[1].f[2] += f2[2] - f1[2];

				myAtoms[2].f[0] += f3[0] - f2[0];
				myAtoms[2].f[1] += f3[1] - f2[1];
				myAtoms[2].f[2] += f3[2] - f2[2];

				myAtoms[3].f[0] -= f3[0];
				myAtoms[3].f[1] -= f3[1];
				myAtoms[3].f[2] -= f3[2];
			}
		}
	}
	private final static double[][] torsionCoeffs = {
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, C, atom.SP3, 0.200,
		 0.270, 0.093},
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, C, atom.SP2, 0.170,
		 0.270, 0.093},
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, C, atom.SP, 0.200,
		 -0.260, 0.093},
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, H, atom.NONE,
		 0.000,
		 0.000, 0.267},
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, O, atom.SP3, 0.100,
		 0.100, 0.180},
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.100,
		 0.400, 0.500},
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP2, 0.000,
		 0.000, 0.400},
		{C, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.100,
		 0.400, 0.500},
		{C, atom.SP2, C, atom.SP3, C, atom.SP3, C, atom.SP2, 2.100,
		 0.270, 0.093},
		{C, atom.SP2, C, atom.SP3, C, atom.SP3, C, atom.SP, 0.000,
		 0.000, 0.093},
		{C, atom.SP2, C, atom.SP3, C, atom.SP3, H, atom.NONE,
		 0.000,
		 0.000, 0.500},
		{C, atom.SP2, C, atom.SP3, C, atom.SP3, O, atom.SP3, 0.000,
		 0.000, 0.180},
		{C, atom.SP2, C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.000,
		 0.000, 0.180},
		{C, atom.SP2, C, atom.SP3, C, atom.SP3, N, atom.SP2, 0.000,
		 0.000, 0.000},
		{C, atom.SP2, C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.000,
		 0.000, 0.180},
		{C, atom.SP, C, atom.SP3, C, atom.SP3, C, atom.SP, 1.000,
		 0.000, 0.093},
		{C, atom.SP, C, atom.SP3, C, atom.SP3, H, atom.NONE, 0.000,
		 0.000, 0.400},
		{C, atom.SP, C, atom.SP3, C, atom.SP3, O, atom.SP3, 0.000,
		 -0.400, 0.180},
		{H, atom.NONE, C, atom.SP3, C, atom.SP3, H, atom.NONE,
		 0.000,
		 0.000, 0.237},
		{H, atom.NONE, C, atom.SP3, C, atom.SP3, O, atom.SP3,
		 0.000,
		 0.000, 0.180},
		{H, atom.NONE, C, atom.SP3, C, atom.SP3, N, atom.SP3,
		 -0.150,
		 0.000, 0.150},
		{H, atom.NONE, C, atom.SP3, C, atom.SP3, N, atom.SP2,
		 0.000,
		 0.000, 0.400},
		{H, atom.NONE, C, atom.SP3, C, atom.SP3, N, atom.SP3,
		 -0.150,
		 0.000, 0.150},
		{H, atom.NONE, C, atom.SP3, C, atom.SP3, O, atom.SP2,
		 0.000,
		 0.000, 0.180},
		{O, atom.SP3, C, atom.SP3, C, atom.SP3, O, atom.SP3, 0.000,
		 -0.600, 0.300},
		{O, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.000,
		 0.000, 0.000},
		{O, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP2, 0.000,
		 0.000, 0.000},
		{O, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP3, 0.000,
		 -0.600, 0.300},
		{N, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP3,
		 -0.400,
		 -1.100, 1.200},
		{N, atom.SP3, C, atom.SP3, C, atom.SP3, N, atom.SP2, 1.170,
		 -1.263, 2.064},
		{N, atom.SP2, C, atom.SP3, C, atom.SP3, N, atom.SP2, 0.000,
		 0.000, -0.500},
		{O, atom.SP2, C, atom.SP3, C, atom.SP3, O, atom.SP2, 0.000,
		 -0.600, 0.300},
		{C, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP3, 0.400,
		 0.030, 0.500},
		{C, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP2,
		 -0.440,
		 0.240, 0.060},
		{C, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP, -0.440,
		 0.240, 0.060},
		{C, atom.SP3, C, atom.SP3, C, atom.SP2, H, atom.NONE,
		 0.000,
		 0.000, 0.010},
		{C, atom.SP3, C, atom.SP3, C, atom.SP2, O, atom.SP3, 0.000,
		 0.000, 0.000},
		{C, atom.SP2, C, atom.SP3, C, atom.SP2, C, atom.SP3, 0.000,
		 0.000, 0.300},
		{C, atom.SP2, C, atom.SP3, C, atom.SP2, C, atom.SP2, 0.100,
		 0.000, 0.500},
		{C, atom.SP2, C, atom.SP3, C, atom.SP2, H, atom.NONE,
		 0.000,
		 0.000, 0.600},
		{C, atom.SP2, C, atom.SP3, C, atom.SP2, O, atom.SP3, 0.000,
		 0.000, 0.000},
		{C, atom.SP, C, atom.SP3, C, atom.SP2, C, atom.SP3, 0.000,
		 0.000, 0.780},
		{C, atom.SP, C, atom.SP3, C, atom.SP2, C, atom.SP2, 0.000,
		 0.000, 0.100},
		{C, atom.SP, C, atom.SP3, C, atom.SP2, H, atom.NONE, 0.000,
		 0.000, 0.780},
		{H, atom.NONE, C, atom.SP3, C, atom.SP2, C, atom.SP3,
		 0.000,
		 0.000, 0.540},
		{H, atom.NONE, C, atom.SP3, C, atom.SP2, C, atom.SP2,
		 0.000,
		 0.000, -0.240},
		{H, atom.NONE, C, atom.SP3, C, atom.SP2, C, atom.SP, 0.000,
		 0.000, -0.240},
		{H, atom.NONE, C, atom.SP3, C, atom.SP2, H, atom.NONE,
		 0.000,
		 0.000, 0.520},
		{H, atom.NONE, C, atom.SP3, C, atom.SP2, O, atom.SP3,
		 0.000,
		 0.000, 0.540},
		{O, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP3, 0.000,
		 0.000, 0.000},
		{O, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP2, 0.000,
		 0.000, 0.000},
		{O, atom.SP3, C, atom.SP3, C, atom.SP2, H, atom.NONE,
		 0.000,
		 0.000, 0.000},
		{N, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP3, 0.000,
		 0.000, 0.000},
		{N, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP2, 0.000,
		 0.000, 0.000},
		{N, atom.SP3, C, atom.SP3, C, atom.SP2, H, atom.NONE,
		 0.000,
		 0.000, 0.000},
		{N, atom.SP3, C, atom.SP3, C, atom.SP2, C, atom.SP2, 0.000,
		 0.000, 0.000},
		{C, atom.SP3, C, atom.SP3, C, atom.SP, C, atom.SP, 0.000,
		 0.001, 0.000},
		{H, atom.NONE, C, atom.SP3, C, atom.SP, C, atom.SP, 0.000,
		 0.001, 0.000},
		{C, atom.SP3, C, atom.SP3, O, atom.SP3, C, atom.SP3, 0.400,
		 0.520, 0.467},
		{C, atom.SP3, C, atom.SP3, O, atom.SP3, C, atom.SP2, 0.000,
		 0.000, 0.400},
		{C, atom.SP3, C, atom.SP3, O, atom.SP3, O, atom.SP3, 0.000,
		 0.000, 0.400},
		{C, atom.SP2, C, atom.SP3, O, atom.SP3, C, atom.SP3, 0.000,
		 0.000, 0.403},
		{H, atom.NONE, C, atom.SP3, O, atom.SP3, C, atom.SP3,
		 0.000,
		 0.000, 0.530},
		{H, atom.NONE, C, atom.SP3, O, atom.SP3, C, atom.SP2,
		 0.000,
		 0.000, 0.530},
		{H, atom.NONE, C, atom.SP3, O, atom.SP3, O, atom.SP3,
		 0.000,
		 0.000, 0.465},
		{O, atom.SP3, C, atom.SP3, O, atom.SP3, C, atom.SP3,
		 -0.170,
		 -1.200, 0.000},
		{O, atom.SP3, C, atom.SP3, O, atom.SP3, O, atom.SP3, 0.000,
		 0.000, 0.403},
		{C, atom.SP3, C, atom.SP3, N, atom.SP3, C, atom.SP3,
		 -0.200,
		 0.730, 0.800},
		{C, atom.SP3, C, atom.SP3, N, atom.SP3, N, atom.SP3,
		 -0.200,
		 0.730, 0.800},
		{C, atom.SP2, C, atom.SP3, N, atom.SP3, C, atom.SP3, 0.000,
		 0.000, 0.000},
		{C, atom.SP2, C, atom.SP3, N, atom.SP3, N, atom.SP3, 0.000,
		 0.000, 0.000},
		{H, atom.NONE, C, atom.SP3, N, atom.SP3, C, atom.SP3,
		 0.000,
		 0.000, 0.520},
		{H, atom.NONE, C, atom.SP3, N, atom.SP3, C, atom.SP2,
		 0.000,
		 0.000, 0.450},
		{H, atom.NONE, C, atom.SP3, N, atom.SP3, N, atom.SP3,
		 0.000,
		 0.000, 0.520},
		{N, atom.SP3, C, atom.SP3, N, atom.SP3, C, atom.SP3, 0.000,
		 0.000, 0.350},
		{N, atom.SP3, C, atom.SP3, N, atom.SP3, N, atom.SP3, 0.000,
		 0.000, 0.350},
		{N, atom.SP3, C, atom.SP3, N, atom.SP3, C, atom.SP3, 0.000,
		 0.000, 0.350},
		{C, atom.SP3, C, atom.SP3, N, atom.SP2, C, atom.SP3, 0.000,
		 0.000, 0.910},
		{C, atom.SP3, C, atom.SP3, N, atom.SP2, C, atom.SP2, 0.000,
		 0.000, 0.000},
		{H, atom.NONE, C, atom.SP3, N, atom.SP2, C, atom.SP3,
		 0.000,
		 0.000, -0.200},
		{H, atom.NONE, C, atom.SP3, N, atom.SP2, C, atom.SP2,
		 0.000,
		 0.000, 0.000},
		{H, atom.NONE, C, atom.SP3, N, atom.SP2, C, atom.SP2,
		 0.000,
		 0.000, 0.650},
		{C, atom.SP3, C, atom.SP3, N, atom.SP3, C, atom.SP3,
		 -0.200,
		 0.730, 0.800},
		{C, atom.SP3, C, atom.SP3, N, atom.SP3, H, atom.NONE,
		 0.000,
		 0.120, 0.100},
		{C, atom.SP2, C, atom.SP3, N, atom.SP3, C, atom.SP3, 0.000,
		 0.000, 0.000},
		{C, atom.SP2, C, atom.SP3, N, atom.SP3, H, atom.NONE,
		 0.000,
		 0.000, 0.000},
		{H, atom.NONE, C, atom.SP3, N, atom.SP3, C, atom.SP3,
		 0.000,
		 0.000, 0.520},
		{H, atom.NONE, C, atom.SP3, N, atom.SP3, H, atom.NONE,
		 0.000,
		 0.000, 0.250},
		{N, atom.SP3, C, atom.SP3, N, atom.SP3, C, atom.SP3, 0.000,
		 0.000, 0.350},
		{C, atom.SP3, C, atom.SP3, O, atom.SP2, C, atom.SP2, 0.000,
		 0.000, 0.400},
		{H, atom.NONE, C, atom.SP3, O, atom.SP2, C, atom.SP2,
		 0.000,
		 0.000, 0.350},
		{C, atom.SP3, C, atom.SP2, C, atom.SP2, C, atom.SP3,
		 -0.100,
		 10.000, 0.000},
		{C, atom.SP3, C, atom.SP2, C, atom.SP2, C, atom.SP2,
		 -0.270,
		 10.000, 0.000},
		{C, atom.SP3, C, atom.SP2, C, atom.SP2, H, atom.NONE,
		 0.000,
		 12.500, 0.000},
		{C, atom.SP3, C, atom.SP2, C, atom.SP2, O, atom.SP3,
		 -1.200,
		 16.250, 0.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, C, atom.SP2,
		 -0.930,
		 8.000, 0.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, C, atom.SP, 0.000,
		 15.000, 0.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, H, atom.NONE,
		 0.000,
		 9.000, -1.060},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, O, atom.SP3, 0.000,
		 16.250, 0.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, N, atom.SP3, 0.000,
		 15.000, 0.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, N, atom.SP2, 0.000,
		 12.000, 0.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP2, O, atom.SP2, 0.000,
		 15.000, 0.000},
		{C, atom.SP, C, atom.SP2, C, atom.SP2, H, atom.NONE, 0.000,
		 15.000, 0.000},
		{H, atom.NONE, C, atom.SP2, C, atom.SP2, H, atom.NONE,
		 0.000,
		 15.000, 0.000},
		{H, atom.NONE, C, atom.SP2, C, atom.SP2, O, atom.SP3,
		 0.000,
		 16.250, 0.000},
		{H, atom.NONE, C, atom.SP2, C, atom.SP2, N, atom.SP3,
		 0.000,
		 15.000, 0.000},
		{H, atom.NONE, C, atom.SP2, C, atom.SP2, N, atom.SP2,
		 0.000,
		 12.000, 0.000},
		{H, atom.NONE, C, atom.SP2, C, atom.SP2, O, atom.SP2,
		 0.000,
		 15.000, 0.000},
		{O, atom.SP3, C, atom.SP2, C, atom.SP2, O, atom.SP3,
		 -2.000,
		 16.250, 0.000},
		{O, atom.SP2, C, atom.SP2, C, atom.SP2, O, atom.SP2,
		 -2.000,
		 15.000, 0.000},
		{C, atom.SP2, C, atom.SP2, C, atom.SP, C, atom.SP, 0.000,
		 0.001, 0.000},
		{C, atom.SP3, C, atom.SP2, O, atom.SP3, C, atom.SP3, 2.300,
		 4.000, 0.000},
		{C, atom.SP3, C, atom.SP2, O, atom.SP3, C, atom.SP2, 0.000,
		 0.000, 0.000},
		{C, atom.SP2, C, atom.SP2, O, atom.SP3, C, atom.SP3, 3.530,
		 2.300, -3.530},
		{C, atom.SP2, C, atom.SP2, O, atom.SP3, C, atom.SP2, 0.000,
		 0.000, 0.000},
		{H, atom.NONE, C, atom.SP2, O, atom.SP3, C, atom.SP3,
		 3.000,
		 3.100, 0.000},
		{H, atom.NONE, C, atom.SP2, O, atom.SP3, C, atom.SP2,
		 0.000,
		 0.000, 0.000},
		{C, atom.SP2, C, atom.SP2, N, atom.SP3, C, atom.SP3,
		 -1.570,
		 3.200, 0.000},
		{H, atom.NONE, C, atom.SP2, N, atom.SP3, C, atom.SP3,
		 1.570,
		 1.690, 0.000},
		{C, atom.SP2, C, atom.SP2, N, atom.SP2, C, atom.SP3, 0.000,
		 2.000, 0.000},
		{C, atom.SP2, C, atom.SP2, N, atom.SP2, C, atom.SP2, 0.000,
		 0.000, 1.490},
		{N, atom.SP2, C, atom.SP2, N, atom.SP2, C, atom.SP3, 0.000,
		 0.000, 0.000},
		{C, atom.SP2, C, atom.SP2, O, atom.SP2, C, atom.SP3, 0.000,
		 9.200, 0.000},
		{C, atom.SP2, C, atom.SP2, O, atom.SP2, C, atom.SP2, 0.000,
		 8.300, -0.800},
		{H, atom.NONE, C, atom.SP2, O, atom.SP2, C, atom.SP3,
		 -0.820,
		 9.200, 3.700},
		{H, atom.NONE, C, atom.SP2, O, atom.SP2, C, atom.SP2,
		 -0.460,
		 2.700, 0.700},
		{C, atom.SP3, C, atom.SP, C, atom.SP, C, atom.SP2, 0.000,
		 0.001, 0.000},
		{C, atom.SP3, C, atom.SP, C, atom.SP, C, atom.SP, 0.000,
		 0.001, 0.000},
		{C, atom.SP2, C, atom.SP, C, atom.SP, C, atom.SP2, 0.000,
		 0.001, 0.000},
		{C, atom.SP2, C, atom.SP, C, atom.SP, C, atom.SP, 0.000,
		 0.001, 0.000},
		{C, atom.SP2, C, atom.SP, C, atom.SP, H, atom.NONE, 0.000,
		 0.001, 0.000},
		{C, atom.SP, C, atom.SP, C, atom.SP, C, atom.SP, 0.000,
		 0.001,
		 0.000},
		{C, atom.SP3, O, atom.SP3, O, atom.SP3, C, atom.SP3, 2.095,
		 -2.155, -0.113},
		{C, atom.SP3, N, atom.SP3, N, atom.SP3, C, atom.SP3, 0.900,
		 -6.800, 0.210},
	};
}
