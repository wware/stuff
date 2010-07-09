/**
 * nitrogen.java
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

import java.awt.Color;

public class nitrogen extends atom {
	public static final String rcsid =
	    "$Id: nitrogen.java,v 1.4 1999/01/13 05:20:54 wware Exp $";
	public nitrogen() {
		double newx[] = { 0.0, 0.0, 0.0 };
		x = newx;
		hybridization = SP3;
	}
	public nitrogen(double x0, double x1, double x2) {
		double newx[] = { x0, x1, x2 };
		x = newx;
		hybridization = SP3;
	}
	public nitrogen(int h, double x0, double x1, double x2) {
		double newx[] = { x0, x1, x2 };
		x = newx;
		hybridization = h;
	}
	public void rehybridize(int hybrid) {
		hybridization = hybrid;
	}
	public void rehybridize()	// based on number of bonds
	{
		switch (currentSigmaBonds()) {
		default:
		case 3:
			hybridization = SP3;
			break;
		case 2:
			hybridization = SP2;
			break;
		case 1:
			hybridization = SP;
			break;
		}
	}
	public double covalentRadius() {
		switch (hybridization) {
		default:
		case SP3:
			return 0.74;
		case SP2:
			return 0.62;
		case SP:
			return 0.55;
		}
	}
	public String name() {
		return "Nitrogen";
	}
	public String symbol() {
		return "N";
	}
	public int atomicNumber() {
		return 7;
	}
	public double mass() {
		return 14.0;
	}
	public Color color() {
		return Color.blue;
	}
	public double vdwEnergy() {
		return 0.447;
	}
	public double vdwRadius() {
		return 1.5;
	}
	public int correctNumBonds() {
		return 3;
	}
}
