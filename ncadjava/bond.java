/**
 * bond.java - definition of a bond
 * Copyright (c) 1998 Will Ware, all rights reserved.
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

public class bond {
	public static final String rcsid =
	    "$Id: bond.java,v 1.7 1999/01/14 04:37:56 wware Exp $";
	private int _order;
	public atom a1, a2;
	public bond(atom atm1, atom atm2) {
		a1 = atm1;
		a2 = atm2;
		_order = 1;
		a1.bonds.addElement(this);
		a2.bonds.addElement(this);
		a1.rehybridize();
		a2.rehybridize();
	} public int order() {
		return _order;
	}
	public void incrOrder() {
		if (_order < 3)
			_order++;
		/* else throw an exception?? */
	}
	public boolean contains(atom atm1) {
		return (a1 == atm1 || a2 == atm1);
	}
	public atom otherAtom(atom atm1) {
		if (atm1 == a1)
			return a2;
		if (atm1 == a2)
			return a1;
		return null;
	}
	public void delete() {
		a1.bonds.removeElement(this);
		a2.bonds.removeElement(this);
	}
	public String repr() {
		return "<Bond " +
		    (new Integer(a1.index())).toString() + " " +
		    (new Integer(a2.index())).toString() + " " +
		    (new Integer(_order)).toString() + ">";
	}
}
