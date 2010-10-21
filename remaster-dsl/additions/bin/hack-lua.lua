#!/bin/lua
-- Written by Jaap Broekhuizen aka Jaapz
-- This script is released under the terms of the GPL2 License

-- aswg.lua
--- A Simple Wget Gui written in murgaLua

require("fltk")

dofile("/etc/init.d/functions5.lua")

prefix = os.getenv("HOME").."/"

--Main
w = fltk:Fl_Window(300,110,"ASWG for DSL")
w:callback(
function(w)
 os.exit(0)
end)

text = fltk:Fl_Box(135,5,30,30,"Fill in the URL of the file you want to download.")

urlInput = fltk:Fl_Input(5,35,290,30)
urlInput:value("http://")

okBtn = fltk:Fl_Return_Button(5,75,50,25,"Ok")
okBtn:callback(
function(okBtn)
 url = urlInput:value()
 os.execute("aterm +tr -geometry 80x4 -e wget --directory-prefix=" ..prefix.. " " ..url)
end)

closeBtn = fltk:Fl_Button(60,75,50,25,"Close")
closeBtn:callback(
function(closeBtn)
 os.exit(0)
end)

optionsBtn = fltk:Fl_Button(115,75,60,25,"Options")
optionsBtn:callback(
function(optionsBtn)
 optionsw = fltk:Fl_Window(300,110,"Options")

 preftext = fltk:Fl_Box(135,5,30,30,"Target Directory:")
 prefixval = fltk:Fl_Input(5,35,290,30)
 prefixval:value(prefix)

 applyBtn = fltk:Fl_Button(100,75,100,25,"Apply")
 applyBtn:callback(
 function(applyBtn)
   prefix = prefixval:value()
   optionsw:hide()
 end)

 optionsw:show()
end)

helpBtn = fltk:Fl_Button(245,75,50,25,"Help")
helpBtn:callback(
function(helpBtn)
 fltk:fl_message([[A Simple Wget GUI, written in murgaLua.

The files will be automatically downloaded to your home directory,
unless otherwise specified. 
Click on the "Options" button to edit the options.]])
end)

w:show()
Fl:run()
