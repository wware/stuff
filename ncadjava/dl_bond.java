/**
 * dlentry.java - entry in a drawing list
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

import java.awt.*;
import java.util.Vector;

public class dl_bond extends dlentry {
	public static final String rcsid =
	    "$Id: dl_bond.java,v 1.3 1999/01/13 05:16:42 wware Exp $";
	private double x1[], r1;	// screen coordinates for first atom
	private double x2[], r2;
	private double x3[];
	private bond myBond;
	// gap between lines in double and triple bonds, in angstroms
	private static final double gap = 0.2;
	public dl_bond(bond b, view v) {
		double m;

		 x3 = new double[3];
		 myBond = b;
		 vw = v;

		 x1 = v.xyzToScreen(myBond.a1.x);
		 r1 = radiusRatio * myBond.a1.covalentRadius() *
		    v.zoomFactor;
		 r1 *= v.perspectiveFactor(x1);

		 x2 = v.xyzToScreen(myBond.a2.x);
		 r2 = radiusRatio * myBond.a2.covalentRadius() *
		    v.zoomFactor;
		 r2 *= v.perspectiveFactor(x2);

		// compute the gap in screen space
		 x3[0] = x1[1] - x2[1] + x1[0];
		 x3[1] = x2[0] - x1[0] + x1[1];
		 x3[2] = x1[2];
		// move it back to atom space to correct the length
		 x3 = v.screenToXyz(x3);
		int i;
		for (i = 0, m = 0.0; i < 3; i++) {
			double dx = x3[i] - myBond.a1.x[i];
			 m += dx * dx;
		} m = gap / Math.sqrt(m);
		for (i = 0; i < 3; i++)
			x3[i] =
			    myBond.a1.x[i] + m * (x3[i] - myBond.a1.x[i]);
		// move it back to screen space
		x3 = v.xyzToScreen(x3);
		for (i = 0; i < 3; i++)
			x3[i] -= x1[i];
	}
	public double zvalue() {
		return (x1[2] + x2[2]) / 2;
	}
	public void quickpaint(Graphics g) {
		Color c1 = myBond.a1.color();
		Color c2 = myBond.a2.color();
		// Gray isn't quite dark enough to look good in a wireframe
		if (c1 == Color.gray)
			c1 = Color.black;
		if (c2 == Color.gray)
			c2 = Color.black;
		drawBondLine(g, c1, c2, x1, x2);
	}
	public void paint(Graphics g) {
		int i;
		double[] v1 = new double[3];
		double[] v2 = new double[3];
		double dv[] = new double[3];
		double lensq = 0.0;
		double[] xdiff = new double[3];
		for (i = 0; i < 3; i++) {
			v1[i] = x1[i];
			v2[i] = x2[i];
			xdiff[i] = v1[i] - v2[i];
			lensq += xdiff[i] * xdiff[i];
		}
		dv[0] = xdiff[1] * (gap / Math.sqrt(lensq));
		dv[1] = -xdiff[0] * (gap / Math.sqrt(lensq));
		dv[2] = 0.0;
		r1 /= Math.sqrt(lensq);
		for (i = 0; i < 3; i++)
			v1[i] -= r1 * xdiff[i];
		r2 /= Math.sqrt(lensq);
		for (i = 0; i < 3; i++)
			v2[i] += r2 * xdiff[i];
		switch (myBond.order()) {
		default:
		case 1:
			drawBondLine(g, Color.black, Color.black, v1, v2);
			break;
		case 2:
			for (i = 0; i < 3; i++) {
				v1[i] += 0.5 * x3[i];
				v2[i] += 0.5 * x3[i];
			}
			drawBondLine(g, Color.black, Color.black, v1, v2);
			for (i = 0; i < 3; i++) {
				v1[i] -= x3[i];
				v2[i] -= x3[i];
			}
			drawBondLine(g, Color.black, Color.black, v1, v2);
			break;
		case 3:
			drawBondLine(g, Color.black, Color.black, v1, v2);
			for (i = 0; i < 3; i++) {
				v1[i] += x3[i];
				v2[i] += x3[i];
			}
			drawBondLine(g, Color.black, Color.black, v1, v2);
			for (i = 0; i < 3; i++) {
				v1[i] -= 2 * x3[i];
				v2[i] -= 2 * x3[i];
			}
			drawBondLine(g, Color.black, Color.black, v1, v2);
		}
	}
}
