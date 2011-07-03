Fooling with GWT
================

I started out trying to do this in Eclipse, and found it was a real pain. It's nice to just grab the GWT SDK zipfile.
http://google-web-toolkit.googlecode.com/files/gwt-2.3.0.zip
Unpack it in the home directory, so now you have /home/wware/gwt-2.3.0. Then you can start an Ant-based project very painlessly::

 ~/gwt-2.3.0/webAppCreator -out Tryit net.willware.tryit.Tryit

There are plenty of samples in the gwt-2.3.0 tree, with plenty of good explanatory material.

To make the source code into an Eclipse project, go to the File menu and choose::

 File -> Import... -> Existing Projects into Workspace

Browse to the "Tryit" directory. Uncheck "Copy projects into workspace" if it is checked and click "Finish".

Even if hacking code in Eclipse, I prefer to run the development server from Ant by typing::

 ant devmode


