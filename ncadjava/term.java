/**
 * term.java - MM2-style energy term, for computing interatomic forces
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

public abstract class term {
	public static final String rcsid =
	    "$Id: term.java,v 1.6 1999/01/13 04:19:04 wware Exp $";
	public atom[] myAtoms;
	public abstract void computeForces();
	public static final double pi = 3.1415926;

	// atomic numbers, used to look up coefficients
	protected final static int H = 1;
	protected final static int C = 6;
	protected final static int N = 7;
	protected final static int O = 8;

	public term() {
		// needed for termList
	}
	// handy vector arithmetic
	    protected double[] crossProduct(double[]x, double[]y) {
		double[] z = new double[3];
		z[0] = x[1] * y[2] - x[2] * y[1];
		z[1] = x[2] * y[0] - x[0] * y[2];
		z[2] = x[0] * y[1] - x[1] * y[0];
		return z;
	}
	protected double dotProduct(double[]x, double[]y) {
		return x[0] * y[0] + x[1] * y[1] + x[2] * y[2];
	}
	protected double veclen(double[]x) {
		return Math.sqrt(dotProduct(x, x));
	}
	protected double[] scalevec(double m, double[]x) {
		double[] z = new double[3];
		int i;
		for (i = 0; i < 3; i++)
			z[i] = m * x[i];
		return z;
	}
	protected double sqrt(double x) {
		if (x <= 0.0)
			return 0.0;
		return Math.sqrt(x);
	}
	protected double acos(double x) {
		if (x >= 1.0)
			return pi / 2;
		if (x <= -1.0)
			return -pi / 2;
		return Math.acos(x);
	}
	public void enumerate(Vector atomList, Vector termList) {
		int i;
		for (i = 0; i < atomList.size(); i++)
			bondChain((atom) atomList.elementAt(i),
				  termLength(), termList);
	}
	protected abstract int termLength();
	protected abstract void buildTerm(Vector v, Vector termList);
	private void bondChain(atom a, int n, Vector termList) {
		Vector v = new Vector();
		v.addElement(a);
		nextBondChain(v, n - 1, termList);
	}
	public String repr() {
		int i;
		if (myAtoms == null)
			return "<Term ???>";
		String s = "<Term";
		for (i = 0; i < myAtoms.length; i++)
			s += " " + myAtoms[i].symbol();
		return s + repr2() + ">";
	}
	protected String repr2() {
		return "";
	}
	private void nextBondChain(Vector v, int n, Vector termList) {
		int i;
		atom a1, a2;
		if (n == 0) {
			buildTerm(v, termList);
			return;
		}
		a1 = (atom) v.elementAt(v.size() - 1);
		for (i = 0; i < a1.bonds.size(); i++) {
			a2 = ((bond) a1.bonds.elementAt(i)).otherAtom(a1);
			if (!v.contains(a2)) {
				v.addElement(a2);
				nextBondChain(v, n - 1, termList);
				v.removeElement(a2);
			}
		}
	}
}
