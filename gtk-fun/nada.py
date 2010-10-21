#!/usr/bin/env python

import pygtk
pygtk.require("2.0")   # necessary???
import gtk
import gtk.glade

class appgui:
    def __init__(self):
        self.bldr = gtk.Builder()
        self.bldr.add_from_file('nada.xml')
        dic = { 'on_button3_click' : self.button3_clicked,
                'on_window1_destroy' : gtk.main_quit }
        self.bldr.connect_signals(dic)
        self.bldr.get_object('window1').show()

    def button3_clicked(self,widget):
        print 'button 3 clicked'
        gtk.main_quit()

appgui()
gtk.main()
