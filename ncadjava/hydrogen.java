/**
 * hydrogen.java
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

public class hydrogen extends atom {
	public static final String rcsid =
	    "$Id: hydrogen.java,v 1.3 1999/01/12 05:38:13 wware Exp $";
	public hydrogen() {
		double newx[] = { 0.0, 0.0, 0.0 };
		x = newx;
		hybridization = NONE;
	}
	public hydrogen(double x0, double x1, double x2) {
		double newx[] = { x0, x1, x2 };
		x = newx;
		hybridization = NONE;
	}
	public void rehybridize(int hybrid) {
	}
	public void rehybridize() {
	}
	public double covalentRadius() {
		return 0.3;
	}
	public String name() {
		return "Hydrogen";
	}
	public String symbol() {
		return "H";
	}
	public int atomicNumber() {
		return 1;
	}
	public double mass() {
		return 1.0;
	}
	public Color color() {
		return Color.white;
	}
	public double vdwEnergy() {
		return 0.382;
	}
	public double vdwRadius() {
		return 1.2;
	}
	public int correctNumBonds() {
		return 1;
	}
}
