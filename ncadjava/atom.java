/**
 * atom.java - definition of an atom, elements are subclasses of atom
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

import java.awt.Color;
import java.util.Vector;

public abstract class atom {
	public static final String rcsid =
	    "$Id: atom.java,v 1.8 1999/01/14 04:37:35 wware Exp $";
	// hybridizations are a virtual enum
	public static final int SP3 = 0;
	public static final int SP2 = 1;
	public static final int SP = 2;
	public static final int NONE = 3;
	private static final String hybridnames[] =
	    { "SP3", "SP2", "SP", "NONE" };

	// these should be defined within elements, as class variables
	public abstract String name();
	public abstract String symbol();
	public abstract int atomicNumber();
	public abstract double mass();
	public abstract Color color();
	public abstract double covalentRadius();
	public abstract double vdwEnergy();
	public abstract double vdwRadius();
	public abstract int correctNumBonds();

	// these should be instance variables
	public int Charge;
	public double fractionalCharge;
	public int hybridization;
	public double[] x;
	public double[] v;
	public double[] f;
	public Vector bonds;
	private group myGroup;

	public atom() {
		hybridization = NONE;
		bonds = new Vector();
		Charge = 0;
		fractionalCharge = 0.0;
		double zvec[] = { 0.0, 0.0, 0.0 };
		x = v = f = zvec;
	}
	public void setGroup(group g) {
		myGroup = g;
	}
	public int index() {
		return myGroup.atomList.indexOf(this);
	}
	public String repr() {
		return "<" + symbol() + " " +
		    hybridnames[hybridization] + " " +
		    (new Double(x[0])).toString() + " " +
		    (new Double(x[1])).toString() + " " +
		    (new Double(x[2])).toString() + ">";
	}
	public void zeroForce() {
		double newf[] = { 0.0, 0.0, 0.0 };
		f = newf;
	}
	public bond bondWith(atom a) {
		int i;
		if (bonds == null)
			return null;
		for (i = 0; i < bonds.size(); i++) {
			bond b = (bond) bonds.elementAt(i);
			if (b.contains(a))
				return b;
		}
		return null;
	}
	public int currentSigmaBonds() {
		int i, total;
		if (bonds == null)
			return 0;
		return bonds.size();
	}
	public int currentNumBonds() {
		int i, total;
		if (bonds == null)
			return 0;
		for (i = total = 0; i < bonds.size(); i++)
			total += ((bond) bonds.elementAt(i)).order();
		return total;
	}
	// overload me, unless I'm hydrogen
	public void rehybridize() {
		hybridization = NONE;
	}
}
