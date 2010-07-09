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

public abstract class dlentry {
	public static final String rcsid =
	    "$Id: dlentry.java,v 1.2 1999/01/12 04:07:30 wware Exp $";
	protected view vw;
	// empirical good-looking multiplier
	protected static final double radiusRatio = 0.6;
	public abstract double zvalue();
	public abstract void quickpaint(Graphics g);
	public abstract void paint(Graphics g);
	public dlentry() {
	} protected int doubleToInt(double x) {
		Double d = new Double(x);
		return d.intValue();
	}
	protected void zsort(Vector v, int lo0, int hi0) {
		int lo = lo0;
		int hi = hi0;
		double mid;
		if (hi0 > lo0) {
			mid = ((dlentry) v.elementAt((lo0 + hi0) / 2)).
			    zvalue();
			while (lo <= hi) {
				while ((lo < hi0) &&
				       (((dlentry) v.elementAt(lo)).
					zvalue() < mid))
					++lo;
				while ((hi > lo0) &&
				       (((dlentry) v.elementAt(hi)).
					zvalue() > mid))
					--hi;
				if (lo <= hi) {
					Object temp;
					temp = v.elementAt(lo);
					v.setElementAt(v.elementAt(hi),
						       lo);
					v.setElementAt(temp, hi);
					++lo;
					--hi;
				}
			}
			if (lo0 < hi)
				zsort(v, lo0, hi);
			if (lo < hi0)
				zsort(v, lo, hi0);
		}
	}
	public void drawLineToAtom(atom a, double x, double y, Graphics g) {
		double[] scr = vw.xyzToScreen(a.x);
		g.setColor(Color.black);
		g.drawLine(doubleToInt(scr[0]),
			   doubleToInt(scr[1]),
			   doubleToInt(x), doubleToInt(y));
	}
	protected void drawBondLine(Graphics g, Color c1, Color c2,
				    double[]v1, double[]v2) {
		Color oldcolor = g.getColor();
		double[] u = new double[2], vmid = new double[3];
		vmid[0] = (v1[0] + v2[0]) / 2;
		vmid[1] = (v1[1] + v2[1]) / 2;
		vmid[2] = (v1[2] + v2[2]) / 2;
		g.setColor(c1);
		g.drawLine(doubleToInt(v1[0]),
			   doubleToInt(v1[1]),
			   doubleToInt(vmid[0]), doubleToInt(vmid[1]));
		g.setColor(c2);
		g.drawLine(doubleToInt(vmid[0]),
			   doubleToInt(vmid[1]),
			   doubleToInt(v2[0]), doubleToInt(v2[1]));
		g.setColor(oldcolor);
	}
	public void quickpaint(Vector v, Graphics g) {
		int i;
		for (i = 0; i < v.size(); i++)
			((dlentry) v.elementAt(i)).quickpaint(g);
	}
	public void paint(Vector v, Graphics g) {
		int i;
		zsort(v, 0, v.size() - 1);
		for (i = 0; i < v.size(); i++)
			((dlentry) v.elementAt(i)).paint(g);
	}
}
