/**
 * nanocad.java
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

import java.applet.*;
import java.awt.*;
import java.lang.Math;

public class nanocad extends Applet {
	public static final String rcsid =
	    "$Id: nanocad.java,v 1.30 1999/01/18 05:13:22 wware Exp $";
	private Button emin;
	private Checkbox showForces;
	private Choice knownStructure;
	private Choice getSetStructure;
	private Choice whichElement;
	private group grp;
	private Panel drawingArea;
	private int xxx, yyy;
	private atom atom1;
	private double atom1z;
	private boolean inDrawingArea, needToRepaint;
	private Label atomInfoBlab;
	private textwin structureWindow;
	private textwin debugWindow;
	private String instructions =
	    "Mouse operations (S=shift, C=control) / pan: S-drag air\n" +
	    "rotate: drag air / zoom: C-drag air horiz / move atom: drag atom\n"
	    +
	    "atom info: click atom / add atom: S-click air / delete atom: S-click atom\n"
	    + "recenter: C-click air / perspective: C-drag air vert\n" +
	    "add bond: S-drag atom to atom / delete bond: C-drag atom to atom\n"
	    + "\n" +
	    "Double and triple bonds: do repeated add-bonds between the same pair of\n"
	    +
	    "atoms. Valences: carbon 4, hydrogen 1, nitrogen 3, oxygen 2\n"
	    + "\n" +
	    "Loading and saving files must be done by cut-and-paste, due to Java's security\n"
	    +
	    "restrictions: an applet cannot read or write files on your hard disk. You can\n"
	    +
	    "load and save molecules in three text formats: NanoCAD's native format, PDB\n"
	    +
	    "format, and XYZ format. Only NanoCAD's native format will preserve info about\n"
	    +
	    "hybridization and bond order, so this is the best way to save molecules between\n"
	    + "sessions.\n" + "\n" +
	    "'Energy Minimize' takes a long time the first time, and any time you change\n"
	    +
	    "the structure (add or delete atoms or bonds), and whenever you load a new\n"
	    +
	    "structure. For any structural change, it needs to re-figure out the energy\n"
	    + "terms.\n" + "\n" +
	    "Long-range forces (electrostatic and van-der-Waals) are only partially\n"
	    + "implemented so far.\n" + "\n" +
	    "==========================================================================\n"
	    + "NanoCAD in Java, January 1999\n" +
	    "Copyright (c) 1997,1998,1999 Will Ware, all rights reserved.\n"
	    + "\n" +
	    "Redistribution and use in source and binary forms, with or without\n"
	    +
	    "modification, are permitted provided that the following conditions\n"
	    + "are met:\n" +
	    "1. Redistributions of source code must retain the above copyright\n"
	    +
	    "   notice, this list of conditions and the following disclaimer.\n"
	    +
	    "2. Redistributions in binary form must reproduce the above copyright\n"
	    +
	    "   notice, this list of conditions and the following disclaimer in the\n"
	    +
	    "   documentation and other materials provided with the distribution.\n"
	    + "\n" +
	    "This software is provided \"as is\" and any express or implied warranties,\n"
	    +
	    "including, but not limited to, the implied warranties of merchantability\n"
	    +
	    "or fitness for any particular purpose are disclaimed. In no event shall\n"
	    +
	    "Will Ware be liable for any direct, indirect, incidental, special,\n"
	    +
	    "exemplary, or consequential damages (including, but not limited to,\n"
	    +
	    "procurement of substitute goods or services; loss of use, data, or\n"
	    +
	    "profits; or business interruption) however caused and on any theory of\n"
	    +
	    "liability, whether in contract, strict liability, or tort (including\n"
	    +
	    "negligence or otherwise) arising in any way out of the use of this\n"
	    +
	    "software, even if advised of the possibility of such damage.\n";

	public void clearScreen() {
		Graphics dg = drawingArea.getGraphics();
		Rectangle r = drawingArea.bounds();
		 dg.setColor(this.getBackground());
		 dg.fillRect(r.x, r.y, r.width, r.height);
	} public void paint(Graphics g) {
		clearScreen();
		grp.paint();
		needToRepaint = false;
	}
	private int x0, y0;	// xxx,yyy tracks the mouse, x0,y0 stands still
	private boolean dragFlag = false;
	private int mouseModifiers;
	public void atomInfo() {
		atomInfoBlab.setBackground(this.getBackground());
		atomInfoBlab.setText("");
	}
	public void atomInfo(String s) {
		atomInfoBlab.setBackground(this.getBackground());
		atomInfoBlab.setText(s);
	}
	public void atomInfo(atom a) {
		if (a == null) {
			atomInfoBlab.setBackground(this.getBackground());
			atomInfoBlab.setText("");
			return;
		}
		a.rehybridize();
		String hinfo = "", bondinfo = "";
		switch (a.hybridization) {
		case atom.SP3:
			hinfo = "sp3";
			break;
		case atom.SP2:
			hinfo = "sp2";
			break;
		case atom.SP:
			hinfo = "sp";
			break;
		}
		if (a.currentNumBonds() < a.correctNumBonds()) {
			bondinfo = "<too few bonds>";
			atomInfoBlab.setBackground(Color.orange);
		} else if (a.currentNumBonds() > a.correctNumBonds()) {
			bondinfo = "<too many bonds>";
			atomInfoBlab.setBackground(Color.orange);
		} else
			atomInfoBlab.setBackground(this.getBackground());
		atomInfoBlab.setText(a.name() + " " +
				     a.symbol() + " " +
				     hinfo + " " + bondinfo);
	}
	public boolean mouseDown(Event e, int x, int y) {
		Rectangle r = drawingArea.bounds();
		inDrawingArea = y < r.height;
		needToRepaint = false;
		double[] scrPos = { x, y, 0 };
		atom1 = grp.selectedAtom(scrPos, true);
		dragFlag = false;
		mouseModifiers =
		    e.modifiers & (Event.SHIFT_MASK | Event.CTRL_MASK);
		if (atom1 != null) {
			double[] atomScrPos = grp.v.xyzToScreen(atom1.x);
			atom1z = atomScrPos[2];
		} else {
			atom1z = 0;
		}
		atomInfo(atom1);
		xxx = x;
		yyy = y;
		x0 = x;
		y0 = y;
		return true;
	}
	public boolean mouseDrag(Event e, int x, int y) {
		boolean movingAtom = false;	// if moving atom, no need for extra line
		if (!dragFlag)
			if (x < x0 - 2 || x > x0 + 2 || y < y0 - 2
			    || y > y0 + 2)
				dragFlag = true;
		if (dragFlag) {
			needToRepaint = true;
			if (atom1 == null) {
				switch (mouseModifiers) {
				default:
					grp.v.rotate(0.01 * (x - xxx),
						     0.01 * (y - yyy));
					break;
				case Event.SHIFT_MASK:
					grp.v.pan(x - xxx, y - yyy);
					break;
				case Event.CTRL_MASK:
					grp.v.zoomFactor *=
					    Math.exp(0.01 * (x - xxx));
					grp.v.perspDist *=
					    Math.exp(0.01 * (y - yyy));
					break;
				}
				clearScreen();
				grp.wireframePaint();
			} else {
				switch (mouseModifiers) {
				default:
					double[] scrPos = { x, y, atom1z };
					atom1.x =
					    grp.v.screenToXyz(scrPos);
					movingAtom = true;
					clearScreen();
					grp.wireframePaint();
					break;
				case Event.SHIFT_MASK:
					clearScreen();
					grp.bubblePaint();
					break;
				case Event.CTRL_MASK:
					clearScreen();
					grp.bubblePaint();
					break;
				}
			}
			xxx = x;
			yyy = y;
			if (atom1 != null && !movingAtom)
				grp.drawLineToAtom(atom1, x, y);
		}
		return true;
	}
	public boolean mouseUp(Event e, int x, int y) {
		double[] scrPos = { x, y, atom1z };
		Graphics g = this.getGraphics();
		atom atom2 = grp.selectedAtom(scrPos, false);
		if (atom1 != null) {
			if (dragFlag) {
				// we dragged on an atom
				switch (mouseModifiers) {
				default:
					atom1.x =
					    grp.v.screenToXyz(scrPos);
					atom2 = atom1;
					break;
				case Event.SHIFT_MASK:
					if (atom1 != atom2
					    && atom1 != null
					    && atom2 != null) {
						// create a new bond if none exists, or increment the
						// order if it does exist.
						grp.addBond(atom1, atom2);
					}
					break;
				case Event.CTRL_MASK:
					if (atom1 != atom2
					    && atom1 != null
					    && atom2 != null)
						grp.deleteBond(atom1,
							       atom2);
					break;
				}
				// give information about the last atom we visited
				atomInfo(atom2);
			} else {
				// we clicked on an atom
				switch (mouseModifiers) {
				default:
					// atom info, do nothing here
					break;
				case Event.SHIFT_MASK:
					if (atom1 != null) {
						atom a = null;
						String ename =
						    whichElement.
						    getSelectedItem();
						if (ename.
						    compareTo("Carbon") ==
						    0)
							a = new
							    carbon(atom1.
								   x[0],
								   atom1.
								   x[1],
								   atom1.
								   x[2]);
						else if (ename.
							 compareTo
							 ("Nitrogen")
							 == 0)
							a = new nitrogen
							    (atom1.x[0],
							     atom1.x[1],
							     atom1.x[2]);
						else if (ename.
							 compareTo
							 ("Oxygen")
							 == 0)
							a = new
							    oxygen(atom1.
								   x[0],
								   atom1.
								   x[1],
								   atom1.
								   x[2]);
						else if (ename.
							 compareTo
							 ("Hydrogen")
							 == 0)
							a = new hydrogen
							    (atom1.x[0],
							     atom1.x[1],
							     atom1.x[2]);
						grp.deleteAtom(atom1);
						if (a != null) {
							grp.addAtom(a);
							atomInfo(a);
						} else
							atomInfo();
					} else
						atomInfo();
					needToRepaint = true;
					atomInfo();
					break;
				case Event.CTRL_MASK:
					// do nothing for undoLastOp
					break;
				}
			}
		} else if (!dragFlag) {
			// we clicked on air
			switch (mouseModifiers) {
			default:
				break;
			case Event.SHIFT_MASK:
				int sp;
				atom a = null;
				needToRepaint = true;
				String ename =
				    whichElement.getSelectedItem();
				if (ename.compareTo("Carbon") == 0)
					a = new carbon();
				else if (ename.compareTo("Nitrogen") == 0)
					a = new nitrogen();
				else if (ename.compareTo("Oxygen") == 0)
					a = new oxygen();
				else if (ename.compareTo("Hydrogen") == 0)
					a = new hydrogen();
				if (a != null) {
					grp.addAtom(a, scrPos);
					atomInfo(a);
				}
				break;
			case Event.CTRL_MASK:
				needToRepaint = true;
				atomInfo();
				grp.updateViewSize();
				grp.centerAtoms();
				break;
			}
		}
		if (needToRepaint)
			repaint();
		return true;
	}
	private void energyMinimize() {
		double scale;
		for (scale = 0.1; scale > 0.0001; scale *= 0.9)
			grp.energyMinimizeStep(scale);
		repaint();
	}
	public boolean action(Event e, Object arg) {
		String s;
		if (e.target == emin) {
			energyMinimize();
			return true;
		} else if (e.target == showForces) {
			grp.setShowForces(showForces.getState());
			repaint();
			return true;
		} else if (e.target == knownStructure) {
			switch (knownStructure.getSelectedIndex()) {
			default:
			case 0:
				grp = new aspirin(drawingArea);
				break;
			case 1:
				grp = new buckyball(drawingArea);
				break;
			case 2:
				grp = new diamond(drawingArea);
				break;
			case 3:
				grp = new diamrod(drawingArea);
				break;
			case 4:
				grp = new propane(drawingArea);
				break;
			case 5:
				grp = new tworings(drawingArea);
				break;
			case 6:
				grp = new water(drawingArea);
				break;
			case 7:
				grp = new group(drawingArea);
				break;
			}
			grp.setShowForces(showForces.getState());
			debugWindow.clear();
			grp.setTextwin(debugWindow);
			grp.updateViewSize();
			repaint();
			return true;
		} else if (e.target == getSetStructure) {
			int i = 0;
			/* Notify the user, if he's about to blow away his unsaved work. */
			String str = "Paste your file in here.";
			if (grp.changedSinceLastSave)
				str +=
				    " To save your work first, hit cancel.";
			str += "\n";
			structureWindow.setVisible(true);
			structureWindow.setGroup(grp);
			grp.fileMode = getSetStructure.getSelectedIndex();
			switch (getSetStructure.getSelectedIndex()) {
			default:
			case 0:	/* Load native format */
				structureWindow.setEditable(true);
				structureWindow.setText(str);
				break;
			case 1:	/* Save native format */
				structureWindow.setEditable(false);
				structureWindow.setText(grp.
							getNativeFormat());
				break;
			case 2:	/* Load PDB */
				structureWindow.setEditable(false);
				structureWindow.
				    setText
				    ("Sorry, this isn't implemented yet.\n");
				break;
			case 3:	/* Save PDB */
				structureWindow.setEditable(false);
				structureWindow.setText(grp.getPDB());
				break;
			case 4:	/* Load XYZ */
				structureWindow.setEditable(true);
				structureWindow.setText(str);
				break;
			case 5:	/* Save XYZ */
				structureWindow.setEditable(false);
				structureWindow.setText(grp.getXYZ());
				break;
			}
			return true;
		}
		return false;
	}
	private void constrain(Container container, Component component,
			       int gridX, int gridY, int gridW, int gridH)
	{
		constrain(container, component, gridX, gridY, gridW, gridH,
			  GridBagConstraints.NONE,
			  GridBagConstraints.NORTHWEST, 0.0, 0.0, 0, 0, 0,
			  0);
	}
	private void constrain(Container container, Component component,
			       int gridX, int gridY, int gridW, int gridH,
			       int fill, int anchor, double weightX,
			       double weightY, int top, int left,
			       int bottom, int right) {
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = gridX;
		c.gridy = gridY;
		c.gridwidth = gridW;
		c.gridheight = gridH;
		c.fill = fill;
		c.anchor = anchor;
		c.weightx = weightX;
		c.weighty = weightY;
		if (top + bottom + left + right > 0)
			c.insets = new Insets(top, left, bottom, right);
		((GridBagLayout) container.getLayout()).
		    setConstraints(component, c);
	}
	public void init() {
		int p = 200;
		this.setBackground(new Color(p, p, p));
		Panel controls = new Panel();
		drawingArea = new Panel();

		structureWindow =
		    new textwin("Structure load/save window", "", false);
		structureWindow.setVisible(false);
		structureWindow.
		    setFont(new Font("Courier", Font.PLAIN, 12));
		debugWindow = new textwin("Debug window", "", false);
		debugWindow.setVisible(false);
		GridBagLayout gridbag = new GridBagLayout();
		this.setLayout(gridbag);

		// this is kind of kludgey, but if I don't put drawingArea first
		// in the gridbag, I don't know how to correctly translate the
		// coordinates between this applet and the drawingArea panel.
		constrain(this, drawingArea, 0, 0, 1, 1,
			  GridBagConstraints.BOTH,
			  GridBagConstraints.NORTH, 1.0, 1.0, 0, 0, 0, 0);

		showForces = new Checkbox("Forces");
		showForces.setState(false);

		emin = new Button("Energy Minimize");

		knownStructure = new Choice();
		knownStructure.addItem("aspirin");
		knownStructure.addItem("buckyball");
		knownStructure.addItem("diamond");
		knownStructure.addItem("diamond rod");
		knownStructure.addItem("propane");
		knownStructure.addItem("two rings");
		knownStructure.addItem("water");
		knownStructure.addItem("-Clear-");
		knownStructure.select("aspirin");

		whichElement = new Choice();
		whichElement.addItem("Carbon");
		whichElement.addItem("Hydrogen");
		whichElement.addItem("Oxygen");
		whichElement.addItem("Nitrogen");
		whichElement.addItem("-Delete-");
		whichElement.select("Carbon");

		getSetStructure = new Choice();
		getSetStructure.addItem("Load Native");
		getSetStructure.addItem("Save Native");
		getSetStructure.addItem("Load PDB");
		getSetStructure.addItem("Save PDB");
		getSetStructure.addItem("Load XYZ");
		getSetStructure.addItem("Save XYZ");
		getSetStructure.select("Load Native");

		grp = new aspirin(drawingArea);
		if (debugWindow.isVisible())
			debugWindow.clear();
		grp.setTextwin(debugWindow);
		grp.v.zoomFactor = 25;

		controls.add(showForces);
		controls.add(emin);
		controls.add(knownStructure);
		controls.add(whichElement);
		controls.add(getSetStructure);
		constrain(this, controls, 0, 1, 1, 1);

		TextArea instrucs = new TextArea(5, 100);
		instrucs.setText(instructions);
		instrucs.setEditable(false);
		constrain(this, instrucs, 0, 2, 1, 1);

		String spaces = "                              ";	/* 30 spaces */
		atomInfoBlab =
		    new Label(spaces + spaces + spaces + spaces + spaces);
		constrain(this, atomInfoBlab, 0, 3, 1, 1);

		this.add(drawingArea);
		this.add(controls);
		this.add(instrucs);
		this.add(atomInfoBlab);

		grp.v.updateSize(600, 300);
		this.repaint();
		this.show();
	}
	public void start() {
		grp.updateViewSize();
	}
}
