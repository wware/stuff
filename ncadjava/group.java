/**
 * group.java - group of atoms and terms
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

import java.awt.*;
import java.lang.Math;
import java.util.Vector;

public class group {
	public static final String rcsid =
	    "$Id: group.java,v 1.24 1999/01/18 04:49:08 wware Exp $";
	public Vector atomList;
	public Vector bondList;
	public Vector termList;
	public boolean changedSinceLastSave = false;
	private boolean needToEnumerateTerms;
	private boolean showForces = false;
	private Vector drawingList;
	public Panel mypanel;
	public view v;
	public double forceMultiplier = 100.0;
	private textwin tw;
	/* these are for formatting strings and numbers */
	private final static int LEFT = 0;
	private final static int RIGHT = 1;
	private String scanString;

	public group() {
		v = new view();
		empty();
	} public group(Panel p) {
		mypanel = p;
		v = new view();
		empty();
	}
	public group(Panel p, textwin d) {
		mypanel = p;
		v = new view();
		empty();
		tw = d;
	}
	public void updateViewSize() {
		Rectangle r = mypanel.bounds();
		v.updateSize(r.width, r.height);
	}
	public void empty() {
		needToEnumerateTerms = false;
		changedSinceLastSave = true;
		atomList = new Vector();
		bondList = new Vector();
		termList = new Vector();
	}
	public void setShowForces(boolean sf) {
		showForces = sf;
	}
	public atom selectedAtom(double[]scrPos, boolean picky) {
		int i;
		atom a, amin;
		double sqDist, minSqDist = 0;
		amin = null;
		for (i = 0; i < atomList.size(); i++) {
			a = (atom) atomList.elementAt(i);
			double[] atomPos = v.xyzToScreen(a.x);
			double dx = atomPos[0] - scrPos[0];
			double dy = atomPos[1] - scrPos[1];
			sqDist = dx * dx + dy * dy;
			if (sqDist < minSqDist || i == 0) {
				minSqDist = sqDist;
				amin = a;
			}
		}
		// if we're picky, we need to be right on top of the atom
		if (!picky
		    || minSqDist < 0.05 * v.zoomFactor * v.zoomFactor)
			return amin;
		else
			return null;
	}
	public void addAtom(atom a) {
		needToEnumerateTerms = true;
		changedSinceLastSave = true;
		atomList.addElement(a);
		a.setGroup(this);
	}
	public void addAtom(atom a, double[]scrPos) {
		needToEnumerateTerms = true;
		changedSinceLastSave = true;
		a.x = v.screenToXyz(scrPos);
		atomList.addElement(a);
		a.setGroup(this);
	}
	public void addAtom(atom a, double x0, double x1, double x2) {
		needToEnumerateTerms = true;
		changedSinceLastSave = true;
		a.x[0] = x0;
		a.x[1] = x1;
		a.x[2] = x2;
		atomList.addElement(a);
		a.setGroup(this);
	}
	public void deleteAtom(atom a) {
		int i;
		if (atomList.size() == 0)
			return;
		needToEnumerateTerms = true;
		changedSinceLastSave = true;
		// remove all bonds connected to the atom
		while (a.bonds.size() > 0) {
			bond b = (bond) a.bonds.elementAt(0);
			b.delete();
			bondList.removeElement(b);
		}
		// remove the atom
		atomList.removeElement(a);
	}
	public void setTextwin(textwin d) {
		int i;
		tw = d;
	}
	public void addBond(atom a1, atom a2) {
		bond b;
		if (a1 == null || a2 == null)
			return;
		b = a1.bondWith(a2);
		if (b != null)
			b.incrOrder();
		else {
			b = new bond(a1, a2);
			bondList.addElement(b);
		}
		needToEnumerateTerms = true;
		changedSinceLastSave = true;
	}
	public void addBond(atom a1, atom a2, int ord) {
		if (a1 == null || a2 == null || ord < 1 || ord > 3)
			return;
		while (true) {
			bond b = a1.bondWith(a2);
			if (b != null) {
				if (b.order() == ord)
					break;
				else
					b.incrOrder();
			} else {
				b = new bond(a1, a2);
				bondList.addElement(b);
			}
		}
		needToEnumerateTerms = true;
		changedSinceLastSave = true;
	}
	public void addBond(int a1, int a2) {
		atom at1 = (atom) atomList.elementAt(a1),
		    at2 = (atom) atomList.elementAt(a2);
		addBond(at1, at2);
	}
	public void addBond(int a1, int a2, int ord) {
		atom at1 = (atom) atomList.elementAt(a1),
		    at2 = (atom) atomList.elementAt(a2);
		addBond(at1, at2, ord);
	}
	public void deleteBond(atom a1, atom a2) {
		bond b = a1.bondWith(a2);
		if (b == null) {
			b = a2.bondWith(a1);
			if (b == null)
				return;
		}
		needToEnumerateTerms = true;
		changedSinceLastSave = true;
		bondList.removeElement(b);
		a1.bonds.removeElement(b);
		a2.bonds.removeElement(b);
		a1.rehybridize();
		a2.rehybridize();
	}
	public void centerAtoms() {
		int i, j;
		atom a;
		double[] x = { 0, 0, 0 };
		for (i = 0; i < atomList.size(); i++) {
			a = (atom) atomList.elementAt(i);
			for (j = 0; j < 3; j++)
				x[j] += a.x[j];
		}
		for (j = 0; j < 3; j++)
			x[j] /= atomList.size();
		for (i = 0; i < atomList.size(); i++) {
			a = (atom) atomList.elementAt(i);
			for (j = 0; j < 3; j++)
				a.x[j] -= x[j];
		}
		updateViewSize();
	}
	public void drawLineToAtom(atom a, double x, double y) {
		dl_atom dummy = new dl_atom(a, v);
		dummy.drawLineToAtom(a, x, y, mypanel.getGraphics());
	}
	public void bubblePaint() {
		int i;
		Vector dlist = new Vector();
		dl_atom dla = null;
		for (i = 0; i < atomList.size(); i++) {
			dla = new dl_atom((atom) atomList.elementAt(i), v);
			dlist.addElement(dla);
		}
		if (dla != null)
			dla.quickpaint(dlist, mypanel.getGraphics());
	}
	public void wireframePaint() {
		int i, j;
		Vector dlist = new Vector();
		dl_atom dla = null;
		dl_bond dlb = null;
		for (i = 0; i < atomList.size(); i++) {
			atom a = (atom) atomList.elementAt(i);
			if (a.currentNumBonds() == 0) {
				dla = new dl_atom(a, v);
				dlist.addElement(dla);
			}
		}
		for (i = 0; i < atomList.size(); i++) {
			atom a1 = (atom) atomList.elementAt(i);
			for (j = 0; j < a1.bonds.size(); j++) {
				bond b = (bond) a1.bonds.elementAt(j);
				atom a2 = b.otherAtom(a1);
				if (a1.x[0] < a2.x[0]) {
					dlb = new dl_bond(b, v);
					dlist.addElement(dlb);
				}
			}
		}
		if (dla != null)
			dla.quickpaint(dlist, mypanel.getGraphics());
		else if (dlb != null)
			dlb.quickpaint(dlist, mypanel.getGraphics());
	}
	public void paint() {
		int i, j;
		dl_atom dla = null;
		dl_bond dlb = null;
		Vector dlist = new Vector();
		if (showForces)
			computeForces();
		for (i = 0; i < atomList.size(); i++) {
			dla = new dl_atom((atom) atomList.elementAt(i), v);
			dlist.addElement(dla);
		}
		for (i = 0; i < atomList.size(); i++) {
			atom a1 = (atom) atomList.elementAt(i);
			for (j = 0; j < a1.bonds.size(); j++) {
				bond b = (bond) a1.bonds.elementAt(j);
				atom a2 = b.otherAtom(a1);
				if (a1.x[0] < a2.x[0]) {
					dlb = new dl_bond(b, v);
					dlist.addElement(dlb);
				}
			}
			if (showForces) {
				dlforce dlf = new dlforce(a1.x, a1.f, v);
				dlf.setForceMultiplier(forceMultiplier);
				dlist.addElement(dlf);
			}
		}
		if (dla != null)
			dla.paint(dlist, mypanel.getGraphics());
		else if (dlb != null)
			dlb.paint(dlist, mypanel.getGraphics());
	}
	private void enumerateTerms() {
		int i, j, k;
		if (!needToEnumerateTerms)
			return;
		needToEnumerateTerms = false;

		for (i = 0; i < atomList.size(); i++)
			((atom) atomList.elementAt(i)).rehybridize();

		termList = new Vector();
		atom a = new carbon();
		term t;
		t = new lterm(a, a);
		t.enumerate(atomList, termList);
		t = new aterm(a, a, a);
		t.enumerate(atomList, termList);
		t = new tterm(a, a, a, a);
		t.enumerate(atomList, termList);
		/*
		   t = new lrterm ();
		   t.enumerate (atomList, termList);
		 */

		if (tw.isVisible()) {
			tw.clear();
			for (i = 0; i < atomList.size(); i++)
				tw.write(((atom) atomList.elementAt(i)).
					 repr() + "\n");
			for (i = 0; i < bondList.size(); i++)
				tw.write(((bond) bondList.elementAt(i)).
					 repr() + "\n");
			/* for (i = 0; i < termList.size()-1; i++) /* skip the LR term */
			for (i = 0; i < termList.size(); i++)
				tw.write(((term) termList.elementAt(i)).
					 repr() + "\n");
		}
	}
	public void computeForces() {
		int i;
		enumerateTerms();
		for (i = 0; i < atomList.size(); i++)
			((atom) atomList.elementAt(i)).zeroForce();
		for (i = 0; i < termList.size(); i++)
			((term) termList.elementAt(i)).computeForces();
	}
	public void energyMinimizeStep(double stepsize) {
		int i;
		computeForces();
		for (i = 0; i < atomList.size(); i++) {
			int j;
			double flensq, m;
			atom a = (atom) atomList.elementAt(i);
			for (j = 0, flensq = 0.0; j < 3; j++)
				flensq += a.f[j] * a.f[j];
			if (flensq > 0.0) {
				m = stepsize / Math.sqrt(flensq);
				for (j = 0; j < 3; j++)
					a.x[j] += m * a.f[j];
			}
		}
	}

	/* Let's support two standards for molecule file formats, PDB and XYZ.
	 * Also, support a "native" file format with hybridization and bond order.
	 * This enum must agree with the order of menu items in nanocad.java.
	 * That's ugly, and should be fixed.
	 */
	public final static int LOADNATIVE = 0;
	public final static int SAVENATIVE = 1;
	public final static int LOADPDB = 2;
	public final static int SAVEPDB = 3;
	public final static int LOADXYZ = 4;
	public final static int SAVEXYZ = 5;
	public int fileMode;
	public void textWindowNotify(String s) {
		int i;
		scanString = s;
		if (scanString.substring(0, 5).equals("Paste")) {
			/* If user left "Paste your..." in the window, skip over it. */
			bagLine();
		}
		skipBlanks();
		switch (fileMode) {
		case LOADNATIVE:
			setNativeFormat();
			break;
		case LOADXYZ:
			setXYZ();
			break;
		case LOADPDB:
			setPDB();
			break;
		default:
			break;
		}
	}
	public String getNativeFormat() {
		int i;
		changedSinceLastSave = false;
		String s = atomList.size() + "\n";
		for (i = 0; i < atomList.size(); i++)
			s += ((atom) atomList.elementAt(i)).repr() + "\n";
		s += bondList.size() + "\n";
		for (i = 0; i < bondList.size(); i++)
			s += ((bond) bondList.elementAt(i)).repr() + "\n";
		return s;
	}
	public void setNativeFormat() {
		int numatoms, numbonds;
		empty();
		numatoms = scanInt();
		bagLine();
		while (numatoms-- > 0) {
			String zs, symbol, hybrid;
			double x, y, z;
			int h = atom.SP3;
			if (scanString.charAt(0) != '<')
				continue;
			scanString = scanString.substring(1);
			symbol = scanWord();
			hybrid = scanWord();
			x = scanDouble();
			y = scanDouble();
			zs = scanWord();
			if (zs.charAt(zs.length() - 1) != '>')
				continue;
			try {
				z = (new Double(zs)).doubleValue();
			}
			catch(Exception e) {
				z = 0.0;
			}
			/* hybridizations */
			if (hybrid.equals("NONE"))
				h = atom.NONE;
			else if (hybrid.equals("SP3"))
				h = atom.SP3;
			else if (hybrid.equals("SP2"))
				h = atom.SP2;
			else if (hybrid.equals("SP"))
				h = atom.SP;
			/* which element */
			if (symbol.equals("H"))
				addAtom(new hydrogen(x, y, z));
			else if (symbol.equals("C"))
				addAtom(new carbon(h, x, y, z));
			else if (symbol.equals("O"))
				addAtom(new oxygen(h, x, y, z));
			else if (symbol.equals("N"))
				addAtom(new nitrogen(h, x, y, z));
			bagLine();
		}

		numbonds = scanInt();
		bagLine();
		while (numbonds-- > 0) {
			String ordstr;
			int index1, index2, order;
			if (!scanString.substring(0, 6).equals("<Bond "))
				continue;
			scanString = scanString.substring(6);
			index1 = scanInt();
			index2 = scanInt();
			ordstr = scanWord();
			if (ordstr.charAt(ordstr.length() - 1) != '>')
				continue;
			ordstr = ordstr.substring(0, ordstr.length() - 1);
			try {
				order = (new Integer(ordstr)).intValue();
			}
			catch(Exception e) {
				order = 1;
			}
			addBond(index1, index2, order);
			bagLine();
		}
	}
	public String getPDB() {
		int i;
		changedSinceLastSave = false;
		String s = "REMARK Here is your molecule\n";
		for (i = 0; i < atomList.size(); i++) {
			atom a = (atom) atomList.elementAt(i);
			s += "ATOM  " +
			    formatInt(i + 1, 5, RIGHT) +
			    "  " +
			    formatString(a.symbol(), 4, LEFT) +
			    "UNK     1    " +
			    formatDouble(a.x[0], 4, 3) +
			    formatDouble(a.x[1], 4, 3) +
			    formatDouble(a.x[2], 4, 3) + "  1.00  0.00 \n";
		}
		if (false)
			for (i = 0; i < bondList.size(); i++) {
				bond b = (bond) bondList.elementAt(i);
				s += "CONECT";
				s += formatInt(b.a1.index() + 1, 5, RIGHT);
				s += formatInt(b.a2.index() + 1, 5, RIGHT);
				s += "\n";
			}
		return s + "END\n";
	}
	public void setPDB() {
		/* not implemented yet */
		System.out.println("setPDB");
	}
	public String getXYZ() {
		int i;
		changedSinceLastSave = false;
		String s = atomList.size() + "\n";
		s += "Molecule Z from the Crab Nebula\n";
		for (i = 0; i < atomList.size(); i++) {
			atom a = (atom) atomList.elementAt(i);
			s += a.symbol();
			s += " ";
			s += a.x[0];
			s += " ";
			s += a.x[1];
			s += " ";
			s += a.x[2];
			s += "\n";
		}
		return s;
	}
	public void setXYZ() {
		empty();
		int numatoms = scanInt();
		String symbol;
		double x, y, z;
		bagLine();
		bagLine();
		while (numatoms-- > 0) {
			symbol = scanWord();
			x = scanDouble();
			y = scanDouble();
			z = scanDouble();
			if (symbol.equals("H"))
				addAtom(new hydrogen(x, y, z));
			else if (symbol.equals("C"))
				addAtom(new carbon(atom.SP3, x, y, z));
			else if (symbol.equals("O"))
				addAtom(new oxygen(atom.SP3, x, y, z));
			else if (symbol.equals("N"))
				addAtom(new nitrogen(atom.SP3, x, y, z));
		}
		guessBondInfo();
	}
	private double scanDouble() {
		double x = 0.0;
		skipBlanks();
		String w = scanWord();
		try {
			x = (new Double(w)).doubleValue();
		}
		catch(Exception e) {
		}
		return x;
	}
	private int scanInt() {
		return (int) scanDouble();
	}
	private String scanWord() {
		String w = "";
		int i, n = scanString.length();
		skipBlanks();
		for (i = 0;
		     i < scanString.length() &&
		     !isBlank(scanString.charAt(i), true); i++);
		w = scanString.substring(0, i);
		scanString = scanString.substring(i);
		return w;
	}
	private void bagLine() {
		int i;
		for (i = 0;
		     i < scanString.length() &&
		     scanString.charAt(i) != '\n'; i++);
		if (scanString.length() > (i + 1))
			scanString = scanString.substring(i + 1);
		else
			scanString = "";
	}
	private void skipBlanks() {
		int i;
		for (i = 0;
		     i < scanString.length() &&
		     isBlank(scanString.charAt(i), false); i++);
		scanString = scanString.substring(i);
	}
	private boolean isBlank(char c, boolean includeNewline) {
		if (includeNewline)
			return c == ' ' || c == '\t' || c == '\n'
			    || c == '\r';
		else
			return c == ' ' || c == '\t';
	}
	private String formatInt(int x, int spaces, int direction) {
		String intstr = "";
		boolean neg = false;
		if (x < 0) {
			neg = true;
			x = -x;
		}
		if (x == 0)
			intstr = "0";
		else
			while (x > 0) {
				intstr =
				    ((char) ((x % 10) + '0')) + intstr;
				x /= 10;
			}
		if (neg)
			intstr = '-' + intstr;
		if (direction == LEFT)
			while (intstr.length() < spaces)
				intstr = intstr + ' ';
		else
			while (intstr.length() < spaces)
				intstr = ' ' + intstr;
		return intstr;
	}
	private String formatUnsignedInt(int x, int spaces, int direction) {
		String intstr = "";
		if (x == 0)
			intstr = "0";
		else
			while (x > 0) {
				intstr =
				    ((char) ((x % 10) + '0')) + intstr;
				x /= 10;
			}
		if (direction == LEFT)
			while (intstr.length() < spaces)
				intstr = intstr + ' ';
		else
			while (intstr.length() < spaces)
				intstr = ' ' + intstr;
		return intstr;
	}
	private String formatString(String x, int spaces, int direction) {
		int i;
		String spc = "";
		for (i = 0; i < spaces - x.length(); i++)
			spc += " ";
		if (direction == LEFT)
			return x + spc;
		else
			return spc + x;
	}
	private String formatFractionalPart(double x, int fracpart) {
		String fracstr = "";
		if (x < 0.0)
			x = -x;
		x -= (int) x;
		while (fracstr.length() < fracpart) {
			int xi;
			x *= 10;
			xi = (int) x;
			x -= xi;
			fracstr = fracstr + (char) (xi + '0');
		}
		return fracstr;
	}
	private String formatDouble(double x, int intpart, int fracpart) {
		boolean neg = false;
		String fraction;
		if (x < 0.0) {
			neg = true;
			x = -x;
		}
		fraction = formatFractionalPart(x, fracpart);
		if (neg) {
			if (((int) x) == 0) {
				String intstr = "-0";
				while (intstr.length() < intpart)
					intstr = " " + intstr;
				return intstr + "." + fraction;
			} else
				return formatInt(-((int) x), intpart,
						 RIGHT) + "." + fraction;
		} else
			return formatInt(((int) x), intpart,
					 RIGHT) + "." + fraction;
	}
	/* This will do for small structures, but may become painful
	 * for big ones, as it is O(N^2). If performance suffers, it
	 * can be improved by sorting atoms into spatial partitions
	 * (an O(N) operation) and then searching for potential bond
	 * mates only in nearby partitions (also O(N)).
	 */
	private void guessBondInfo() {
		int i, j;
		atom a1, a2;
		for (i = 0; i < atomList.size() - 1; i++) {
			a1 = (atom) atomList.elementAt(i);
			for (j = i + 1; j < atomList.size(); j++) {
				int k;
				double d = 0.0;
				a2 = (atom) atomList.elementAt(j);
				for (k = 0; k < 3; k++)
					d += (a1.x[k] -
					      a2.x[k]) * (a1.x[k] -
							  a2.x[k]);
				if (Math.sqrt(d) <
				    (a1.covalentRadius() +
				     a2.covalentRadius() + 0.5))
					addBond(a1, a2);
			}
		}
	}
}
