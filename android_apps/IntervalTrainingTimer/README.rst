Remaining TODO items

- Use Mobclix for advertising.

Some URLs:

- http://developer.android.com/guide/developing/other-ide.html
- http://developer.android.com/guide/developing/tools/emulator.html

You don't need Eclipse to do all this stuff::

 # Set up a path to make tools easily reachable.
 export PATH=$PATH:/opt/android-sdk-linux_86/tools:/opt/android-sdk-linux_86/platform-tools
 
 # Create a Hello-World skeleton for an Android app.
 # Choose target from options shown by "android list targets".
 export APPNAME=IntervalTrainer
 android create project --name ${APPNAME} --target 5 --path ./${APPNAME} \
   --package net.willware.${APPNAME} --activity ${APPNAME}
 cd ${APPNAME}/
 ant debug         # build a debug version of the app

Next I used the GUI presented by the "android" tool to create an emulator and named it
"HTC-Droid-Incredible". Now I can run the code, but I need to be in the app's
directory to do it::

 emulator -avd HTC-Droid-Incredible    # Run the emulator
 adb -s emulator-5554 install bin/${APPNAME}-debug.apk   # run my app in the emulator

Signing the app is a big deal for getting into the market. Here's some stuff about
dealing with keys, certificates, signatures, etc.

http://www.sslshopper.com/article-most-common-java-keytool-keystore-commands.html
