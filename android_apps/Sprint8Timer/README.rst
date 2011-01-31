Remaining TODO items

- Make sure the AdView works right, check library path, but I think the issue is with
  the fact that I'm not yet in the market.

Some URLs:

- http://developer.android.com/guide/developing/other-ide.html
- http://developer.android.com/guide/developing/tools/emulator.html

You don't need Eclipse to do all this stuff::

 # Set up a path to make tools easily reachable.
 export PATH=$PATH:/opt/android-sdk-linux_86/tools:/opt/android-sdk-linux_86/platform-tools
 
 # Create a Hello-World skeleton for an Android app.
 # Choose target from options shown by "android list targets".
 android create project --name Sprint8Timer --target 5 --path ./Sprint8Timer \
   --package net.willware.Sprint8Timer --activity Sprint8Timer
 cd Sprint8Timer/
 ant debug         # build a debug version of the app

Next I used the GUI presented by the "android" tool to create an emulator and named it
"HTC-Droid-Incredible". Now I can run the code, but I need to be in the Sprint8Timer
directory to do it::

 emulator -avd HTC-Droid-Incredible    # Run the emulator
 adb -s emulator-5554 install bin/Sprint8Timer-debug.apk   # run my app in the emulator

Signing the app is a big deal for getting into the market. Here's some stuff about
dealing with keys, certificates, signatures, etc.

http://www.sslshopper.com/article-most-common-java-keytool-keystore-commands.html
