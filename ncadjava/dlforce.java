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
 * 3. All advertising materials mentioning features or use of this software
 *    or its derived works must display the following acknowledgement:
 *      This product includes software developed by Will Ware.
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

public class dlforce extends dlentry {
	public static final String rcsid =
	    "$Id: dlforce.java,v 1.2 1999/01/12 04:07:42 wware Exp $";
	private static Color force_color = Color.green;
	private double begin[], end[], orig[], f[], zval;
	private double forceMultiplier;
	private double arrowHeadSize = 5;
	private final static double sqrtHalf = Math.sqrt(0.5);
	public dlforce(double origin[], double f0[], view v) {
		int i;
		 orig = new double[3];
		 f = new double[3];
		 vw = v;
		 begin = v.xyzToScreen(origin);
		for (i = 0; i < 3; i++) {
			f[i] = f0[i];
			orig[i] = origin[i];
		}
		setForceMultiplier(10.0);
	}
	public void setForceMultiplier(double fm) {
		int i;
		end = new double[3];
		forceMultiplier = fm;
		for (i = 0; i < 3; i++)
			end[i] = orig[i] + f[i] * forceMultiplier;
		end = vw.xyzToScreen(end);
		zval = (begin[2] + end[2]) / 2;
	}
	public double zvalue() {
		return zval;
	}
	public void quickpaint(Graphics g) {
		drawBondLine(g, force_color, force_color, begin, end);
		double[] u = new double[3];
		double[] v = new double[3];
		u[0] = end[1] - begin[1];
		u[1] = begin[0] - end[0];
		v[0] = end[0] - begin[0];
		v[1] = end[1] - begin[1];
		double m = Math.sqrt(u[0] * u[0] + u[1] * u[1]);
		if (m > arrowHeadSize)
			m = sqrtHalf * arrowHeadSize / m;
		else
			m = sqrtHalf;
		u[0] *= m;
		u[1] *= m;
		v[0] *= m;
		v[1] *= m;
		double m1 = Math.sqrt(u[0] * u[0] + u[1] * u[1]);
		g.setColor(force_color);
		g.drawLine(doubleToInt(end[0]),
			   doubleToInt(end[1]),
			   doubleToInt(end[0] + u[0] - v[0]),
			   doubleToInt(end[1] + u[1] - v[1]));
		g.drawLine(doubleToInt(end[0]),
			   doubleToInt(end[1]),
			   doubleToInt(end[0] - u[0] - v[0]),
			   doubleToInt(end[1] - u[1] - v[1]));
	}
	public void paint(Graphics g) {
		/* dumb for now, smarten it up later */
		quickpaint(g);
	}
}
