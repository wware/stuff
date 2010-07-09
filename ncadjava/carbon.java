/**
 * carbon.java
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

import java.awt.Color;

public class carbon extends atom {
	public static final String rcsid =
	    "$Id: carbon.java,v 1.7 1999/01/17 17:13:18 wware Exp $";
	public carbon() {
		double newx[] = { 0.0, 0.0, 0.0 };
		x = newx;
		hybridization = SP3;
	}
	public carbon(double x0, double x1, double x2) {
		double newx[] = { x0, x1, x2 };
		x = newx;
		hybridization = SP3;
	}
	public carbon(int h, double x0, double x1, double x2) {
		double newx[] = { x0, x1, x2 };
		x = newx;
		hybridization = h;
	}
	public void rehybridize(int hybrid) {
		hybridization = hybrid;
	}
	public void rehybridize()	// based on number of bonds
	{
		int i, doubles = 0;
		for (i = 0; bonds != null && i < bonds.size(); i++) {
			switch (((bond) bonds.elementAt(i)).order()) {
			case 2:
				doubles++;
				break;
			case 3:
				hybridization = SP;
				return;
			}
		}
		switch (doubles) {
		case 0:
			hybridization = SP3;
			break;
		case 1:
			hybridization = SP2;
			break;
		default:
			hybridization = SP;
			break;
		}
	}
	public double covalentRadius() {
		switch (hybridization) {
		default:
		case SP3:
			return 0.77;
		case SP2:
			return 0.67;
		case SP:
			return 0.6;
		}
	}
	public String name() {
		return "Carbon";
	}
	public String symbol() {
		return "C";
	}
	public int atomicNumber() {
		return 6;
	}
	public double mass() {
		return 12.0;
	}
	public Color color() {
		return Color.gray;
	}
	public double vdwEnergy() {
		return 0.357;
	}
	public double vdwRadius() {
		return 1.85;
	}
	public int correctNumBonds() {
		return 4;
	}
}
