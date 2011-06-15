/**
 * wrapper.java - Java application wrapper for NanoCAD applet
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

public class wrapper extends Frame {
	public static final String rcsid =
	    "$Id: wrapper.java,v 1.8 1999/01/15 04:24:54 wware Exp $";
	public FileDialog fd;
	public nanocad nc;
	public Menu menu;
	public MenuBar menubar;
	public wrapper() {
		this.setTitle("NanoCAD in Java - January 1999");
		this.resize(600, 550);

		nc = new nanocad();
		this.add("Center", nc);
		nc.init();

		menu = new Menu("File");
		menubar = new MenuBar();
		menu.add("Load");
		menu.add("Save");
		menu.add("Quit");
		menubar.add(menu);
		this.setMenuBar(menubar);
	} public boolean handleEvent(Event e) {
		if (e.id == Event.WINDOW_DESTROY) {
			System.exit(0);
			return true;
		}
		if (e.arg == "Quit") {
			System.exit(0);
			return true;
		}
		if (e.arg == "Load") {
			fd = new FileDialog(this,
					    "File to Load",
					    FileDialog.LOAD);
			fd.show();
			/* read structure from PDB file or XYZ file */
			return true;
		} else if (e.arg == "Save") {
			fd = new FileDialog(this,
					    "File to Save",
					    FileDialog.SAVE);
			fd.show();
			/* write structure to PDB file or XYZ file */
			return true;
		}
		return false;
	}
	public static void main(String[]args) {
		wrapper w = new wrapper();
		w.show();
	}
}
