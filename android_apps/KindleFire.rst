Android app development for the Kindle Fire
===========================================

General links (some pieces of these are dated or deprecated)

* http://liliputing.com/2011/11/how-to-sideload-apps-even-the-android-market-on-the-amazon-kindle-fire.html - the bit about the Go Launcher EX is no longer accurate
* http://www.disgruntledrats.com/?p=27 - Getting Ant to Work with Android
* http://blog.actlocalmedia.com/2011/11/developing-on-kindle-fire.html
* http://www.tomsguide.com/us/Amazon-Kindle-Fire-Android-Root-death2all110,news-13219.html
* http://stackoverflow.com/questions/2757107/developing-for-android-in-eclipse-r-java-not-generating
* http://www.alittlemadness.com/2010/05/31/setting-up-an-android-project-build/
* http://www.javahotchocolate.com/tutorials/android.html

Google's Android dev info

* http://developer.android.com/guide/index.html
* http://developer.android.com/guide/developing/tools/adb.html
* http://developer.android.com/sdk/index.html
* http://developer.android.com/sdk/installing.html

Amazon's developer info

* http://www.amazonappstoredev.com/
* https://developer.amazon.com/help/faq.html
* https://developer.amazon.com/welcome.html

Here's how to get an APK installed on the Fire in Ubuntu.
This DOES NOT require rooting the Fire, which sounds a little dangerous to me.
Build the APK - my preference is to do this with Ant, which requires ant1.8.
DO think about unit tests for Android apps.

::

 cd /opt
 tar xfz .../android-sdk_r15-linux.tgz 

Make sure you're using Ant 1.8. By default, Ubuntu Lucid likes Ant 1.7.1 but Android's tools don't. You'll need to
update the build.xml file as follows::

 android update project -p <projectdir>

Check these files into source control: build.xml, project.properties, proguard.cfg. Do not put local.properties or
build.properties in source control. If you have default.properties in source control, remove it.

Next add the line "0x1949" to the ~/.android/adb_usb.ini file. Do "adb kill-server" and "adb devices", you should see::

 $ export PATH=$PATH:/opt/android-sdk-linux/platform-tools:/opt/android-sdk-linux/tools
 $ adb devices
 List of devices attached
 6EFA000600000001	device

Now build the APK and copy it onto the device as follows::

 $ adb install .../bin/YourAppNameHere-debug.apk 
 4237 KB/s (24135 bytes in 0.005s)
         pkg: /data/local/tmp/YourAppNameHere-debug.apk
 Success

After I installed my app (LocationSender-debug.apk), I wanted to make sure I could uninstall it, and found I needed to
use the Java package name::

 $ adb uninstall net.willware.locationsender
 Success

and the following "adb logcat" activity followed::

 I/PackageManager( 1385): Removing non-system package:net.willware.locationsender
 D/PackageManager( 1385): Removing package net.willware.locationsender
 D/PackageManager( 1385):   Activities: net.willware.locationsender.LocationSender
 I/ActivityManager( 1385): Force stopping package net.willware.locationsender uid=10029
 I/ActivityManager( 1385): Force stopping package net.willware.locationsender uid=10029
 D/dalvikvm( 1385): GC_EXPLICIT freed 1470K, 51% free 6268K/12679K, external 7011K/8755K, paused 65ms
 D/dalvikvm( 1385): GC_EXPLICIT freed 110K, 52% free 6180K/12679K, external 7011K/8755K, paused 63ms
 I/installd( 1294): unlink /data/dalvik-cache/data@app@net.willware.locationsender-1.apk@classes.dex
