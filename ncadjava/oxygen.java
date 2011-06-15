/**
 * oxygen.java
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

public class oxygen extends atom {
	public static final String rcsid =
	    "$Id: oxygen.java,v 1.4 1999/01/13 05:21:09 wware Exp $";
	public static String ename = "Oxygen";
	public static String symbol = "O";
	public static int atomicNumber = 8;
	public static double mass = 16.0;
	public static Color color = Color.red;
	public static double vdwRadius = 1.4;
	public static int expectedNumBonds = 2;
	public oxygen() {
		double newx[] = { 0.0, 0.0, 0.0 };
		x = newx;
		hybridization = SP3;
	}
	public oxygen(double x0, double x1, double x2) {
		double newx[] = { x0, x1, x2 };
		x = newx;
		hybridization = SP3;
	}
	public oxygen(int h, double x0, double x1, double x2) {
		double newx[] = { x0, x1, x2 };
		x = newx;
		hybridization = h;
	}
	public void rehybridize(int hybrid) {
		hybridization = hybrid;
	}
	public void rehybridize() {
		switch (currentSigmaBonds()) {
		default:
		case 2:
			hybridization = SP3;
			break;
		case 1:
			hybridization = SP2;
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
		return "Oxygen";
	}
	public String symbol() {
		return "O";
	}
	public int atomicNumber() {
		return 8;
	}
	public double mass() {
		return 16.0;
	}
	public Color color() {
		return Color.red;
	}
	public double vdwEnergy() {
		if (hybridization == SP3)
			return 0.406;
		else
			return 0.536;
	}
	public double vdwRadius() {
		return 1.4;
	}
	public int correctNumBonds() {
		return 2;
	}
}
