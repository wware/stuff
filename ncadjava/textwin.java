/**
 * textwin.java - pop up a new frame to read or write text
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

import java.awt.*;

public class textwin extends Frame {
	protected String contents = "Here's a debug window\n";
	protected TextArea textWindow;
	protected boolean editable, visible;
	private Button done, cancel;
	private group grp;
	public textwin() {
		/* this one is only for inheritance purposes */
	}
	public textwin(String title, String initialContent, boolean edtbl) {
		Panel p1 = new Panel();
		Panel p2 = new Panel();
		editable = edtbl;
		visible = false;
		setTitle(title);
		contents = initialContent;
		textWindow = new TextArea(20, 80);
		textWindow.setText(contents);
		textWindow.setEditable(editable);
		p1.add("North", textWindow);
		done = new Button("Done");
		p2.add("West", done);
		cancel = new Button("Cancel");
		p2.add("East", cancel);
		add("North", p1);
		add("South", p2);
		resize(800, 400);
	}
	public boolean handleEvent(Event e) {
		if (e.arg == "Done") {
			update();
			setVisible(false);
			if (grp != null)
				grp.textWindowNotify(contents);
			return true;
		}
		if (e.arg == "Cancel") {
			update();
			setVisible(false);
			return true;
		}
		return false;
	}
	public void setGroup(group g) {
		grp = g;
	}
	public void setVisible(boolean v) {
		visible = v;
		if (v)
			show();
		else
			hide();
	}
	public boolean isVisible() {
		return visible;
	}
	public void setEditable(boolean e) {
		editable = e;
		textWindow.setEditable(editable);
	}
	public boolean isEditable() {
		return editable;
	}
	public void update() {
		if (editable)
			contents = textWindow.getText();
	}
	public void setText(String s) {
		clear();
		write(s);
	}
	public String getText() {
		update();
		return contents;
	}
	public void clear() {
		contents = "";
		textWindow.setText(contents);
	}
	public void write(String s) {
		contents = contents + s;
		textWindow.setText(contents);
	}
	public void write(int x) {
		contents = contents + (new Integer(x)).toString();
		textWindow.setText(contents);
	}
	public void write(double x) {
		contents = contents + (new Double(x)).toString();
		textWindow.setText(contents);
	}
}
