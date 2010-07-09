/**
 * dl_atom.java - entry in a drawing list, for drawing an atom
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

import java.awt.*;
import java.util.Vector;

public class dl_atom extends dlentry {
	public static final String rcsid =
	    "$Id: dl_atom.java,v 1.3 1999/01/13 05:26:28 wware Exp $";
	private double x1[], r1;	// screen coordinates for first atom
	private atom atm1;
	private boolean bogus;
	public dl_atom(atom a, view v) {
		atm1 = a;
		vw = v;
		x1 = v.xyzToScreen(a.x);
		r1 = radiusRatio * a.covalentRadius() * v.zoomFactor;
		r1 *= v.perspectiveFactor(x1);
		x1[0] -= r1;
		x1[1] -= r1;
		bogus = a.currentNumBonds() != a.correctNumBonds();
	} public double zvalue() {
		return x1[2];
	}
	public void quickpaint(Graphics g) {
		g.drawOval(doubleToInt(x1[0]), doubleToInt(x1[1]),
			   doubleToInt(2 * r1), doubleToInt(2 * r1));
	}
	public void paint(Graphics g) {
		int i;
		g.setColor(atm1.color());
		g.fillOval(doubleToInt(x1[0]), doubleToInt(x1[1]),
			   doubleToInt(2 * r1), doubleToInt(2 * r1));
		if (bogus)
			g.setColor(Color.orange);
		else
			g.setColor(Color.black);
		g.drawOval(doubleToInt(x1[0]), doubleToInt(x1[1]),
			   doubleToInt(2 * r1), doubleToInt(2 * r1));
	}
}
