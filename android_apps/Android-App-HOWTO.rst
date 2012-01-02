How to write and deploy an Android app
======================================

I often get ideas for Android apps, and then I get bogged down thinking,
there's an awful lot of things to remember. So this document attempts to
gather all that stuff in one place and generally make the process of writing,
testing, and deploying apps easier. Let's start with pointers to usefule
information.

* `Android Developer's Guide`_

  - `Application Fundamentals`_

* `Javadoc for Android classes`_
* a `lengthy article`_ on how to develop for Android; heavy emphasis on using
  Eclipse.
* the `official thing`_ on how to develop for Android, with subsections:

  - `Managing virtual devices`_
  - `Using real hardware devices`_
  - `Managing projects`_
  - `Building and running`_
  - `Debugging`_
  - `Testing`_
  - `Tools`_ including `ADB`_

* `Developer resources`_ include sample code, articles, and tutorials

.. _`Android Developer's Guide`: http://developer.android.com/guide/index.html
.. _`Application Fundamentals`: http://developer.android.com/guide/topics/fundamentals.html
.. _`Javadoc for Android classes`: http://developer.android.com/reference/packages.html
.. _`lengthy article`: http://www.vogella.de/articles/Android/article.html
.. _`official thing`: http://developer.android.com/guide/developing/index.html

.. _`Managing virtual devices`: http://developer.android.com/guide/developing/devices/index.html
.. _`Using real hardware devices`: http://developer.android.com/guide/developing/device.html
.. _`Managing projects`: http://developer.android.com/guide/developing/projects/index.html
.. _`Building and running`: http://developer.android.com/guide/developing/building/index.html
.. _`Debugging`: http://developer.android.com/guide/developing/debugging/index.html
.. _`Testing`: http://developer.android.com/guide/developing/testing/index.html
.. _`Tools`: http://developer.android.com/guide/developing/tools/index.html
.. _`ADB`: http://developer.android.com/guide/developing/tools/adb.html

.. _`Developer resources`: http://developer.android.com/resources/index.html

Kindle Fire
-----------

General links (some pieces of these are dated or deprecated)

* http://liliputing.com/2011/11/how-to-sideload-apps-even-the-android-market-on-the-amazon-kindle-fire.html - the bit about the Go Launcher EX is no longer accurate
* http://www.disgruntledrats.com/?p=27 - Getting Ant to Work with Android
* http://blog.actlocalmedia.com/2011/11/developing-on-kindle-fire.html
* http://www.tomsguide.com/us/Amazon-Kindle-Fire-Android-Root-death2all110,news-13219.html
* http://stackoverflow.com/questions/2757107/developing-for-android-in-eclipse-r-java-not-generating
* http://www.alittlemadness.com/2010/05/31/setting-up-an-android-project-build/
* http://www.javahotchocolate.com/tutorials/android.html

Amazon's developer info

* http://www.amazonappstoredev.com/
* https://developer.amazon.com/help/faq.html
* https://developer.amazon.com/welcome.html

Here's how to get an APK installed on the Fire in Ubuntu. Having set up your
environment (see below), add the line "0x1949" to the ~/.android/adb_usb.ini
file. Plug the Fire into the USB cable and then::

 $ adb kill-server
 $ adb devices
 List of devices attached
 6EFA000600000001	device

Setting up your environment
---------------------------

This DOES NOT require rooting the Fire, which sounds a little dangerous to me.
Build the APK - my preference is to do this with Ant, which requires ant1.8.
DO think about unit tests for Android apps.

::

 cd /opt
 tar xfz .../android-sdk_r15-linux.tgz 
 export PATH=$PATH:/opt/android-sdk-linux/platform-tools:/opt/android-sdk-linux/tools
 # also add the PATH line to your .bashrc file

Make sure you're using Ant 1.8. By default, Ubuntu Lucid likes Ant 1.7.1 but
Android's tools don't.



File Layout
-----------

If you have old projects around, you'll need to update them::

 android update project -p <projectdir>

Check these files into source control: build.xml, project.properties,
proguard.cfg. Do not put local.properties or build.properties in source
control. If you have default.properties in source control, remove it.

For a new project, go into the project directory (in this case,
stuff/android_apps/MyAndroidApp) and type::

 # Choose target from options shown by "android list targets".
 export APPNAME=MyAndroidApp
 android create project --name ${APPNAME} --target android-10 --path . \
   --package net.willware.${APPNAME} --activity ${APPNAME}

There is a standard layout to files in an Android project. Starting at the
root directory:

* `AndroidManifest.xml`_
* README.rst - optional but often helpful
* ant.properties - maybe optional?
* `build.xml`_ - because I'm really more of a command-line guy
* local.properties - specifies sdk.dir, location of SDK, do not check into
  source control
* project.properties - specifies the target
* src/net/willware/Foo/Bar.java - source files go down here
* res/values/`strings.xml`_
* res/layout/`main.xml`_
* You'll want a square icon with a `transparent background`_. Scale this to
  three different resolutions.

  * res/drawable-hdpi/icon.png: 72x72
  * res/drawable-mdpi/icon.png: 48x48
  * res/drawable-ldpi/icon.png: 36x36

.. image:: CryptoThing/res/drawable-hdpi/icon.png
   :height: 72
   :width: 72
   :alt: 72x72
.. image:: CryptoThing/res/drawable-mdpi/icon.png
   :height: 48
   :width: 48
   :alt: 48x48
.. image:: CryptoThing/res/drawable-ldpi/icon.png
   :height: 36
   :width: 36
   :alt: 36x36

.. _`AndroidManifest.xml`: http://developer.android.com/guide/topics/manifest/manifest-intro.html
.. _`build.xml`: http://developer.android.com/resources/tutorials/hello-world.html#noeclipse
.. _`main.xml`: http://developer.android.com/resources/tutorials/hello-world.html#upgrading
.. _`strings.xml`: http://developer.android.com/guide/topics/resources/string-resource.html
.. _`transparent background`: http://docs.gimp.org/en/gimp-using-web-transparency.html

Running the simulator
---------------------

First build your app, and find out what emulators are available::

 ant debug
 android list avd

If you need an emulator that isn't on the list, create it with `these
instructions`_, or use the GUI presented by running "android" with no
arguments. I want to use one called "HtcIncredible" which already exists::

 emulator -avd HtcIncredible &                           # start the emulator
 # keep the emulator in GUI focus, it STILL takes forever
 adb devices                                             # make sure ADB sees emulator
 # periodically check "adb devices" until it stops saying "offline", then...
 adb -s emulator-5554 install bin/${APPNAME}-debug.apk   # run my app in the emulator

.. _`these instructions`: http://developer.android.com/guide/developing/devices/managing-avds-cmdline.html#AVDCmdLine

If you have only one emulator or physical device connected, you don't need
to specify the "-s" argument::

 $ adb install bin/${APPNAME}-debug.apk 
 4237 KB/s (24135 bytes in 0.005s)
         pkg: /data/local/tmp/${APPNAME}-debug.apk
 Success

After I installed my app (LocationSender-debug.apk), I wanted to make sure I
could uninstall it, and found I needed to use the Java package name. The
"uninstall" command works sometimes and not other timesw, and I'm not sure
whether it depends upon the target involved, or the SDK version::

 $ adb uninstall net.willware.locationsender
 Success

When it works, the following "adb logcat" activity follows::

 I/PackageManager( 1385): Removing non-system package:net.willware.locationsender
 D/PackageManager( 1385): Removing package net.willware.locationsender
 D/PackageManager( 1385):   Activities: net.willware.locationsender.LocationSender
 I/ActivityManager( 1385): Force stopping package net.willware.locationsender uid=10029
 I/ActivityManager( 1385): Force stopping package net.willware.locationsender uid=10029
 D/dalvikvm( 1385): GC_EXPLICIT freed 1470K, 51% free 6268K/12679K, external 7011K/8755K, paused 65ms
 D/dalvikvm( 1385): GC_EXPLICIT freed 110K, 52% free 6180K/12679K, external 7011K/8755K, paused 63ms
 I/installd( 1294): unlink /data/dalvik-cache/data@app@net.willware.locationsender-1.apk@classes.dex


Signing the app is a big deal for getting into the market. Here's some stuff about
dealing with keys, certificates, signatures, etc.
http://www.sslshopper.com/article-most-common-java-keytool-keystore-commands.html

Menus
-----

To set up a menu, specify a menu file such as res/menu/foo.xml::

 <?xml version="1.0" encoding="utf-8"?>
 <menu xmlns:android="http://schemas.android.com/apk/res/android">
     <item android:id="@+id/info_menu_item"
           android:icon="@drawable/ic_menu_info_details"
           android:title="About..." />
     <item android:id="@+id/eula_menu_item"
           android:icon="@drawable/ic_menu_attachment"
           android:title="EULA" />
     <item android:id="@+id/settings_menu_item"
           android:icon="@drawable/ic_menu_preferences"
           android:title="Settings" />
 </menu>

Each menu option requires an icon, which should be 48x48 pixels with a
`transparent background`_. These icons live in the res/drawable directory
though presumably they could live elsewhere.

.. image:: IntervalTrainer/res/drawable/ic_menu_preferences.png
   :height: 48
   :width: 48
   :alt: 48x48

If the main activity overloads the onCreateOptionsMenu method, that will
create the menu at the bottom of the screen and comply with Android
conventions about how menus work.

When the user selects a menu item, that invokes the onOptionsItemSelected
method. In this example, the "Settings" option kicks off a new activity,
which is discussed in more detail later::

 import android.view.MenuInflater;
 import android.view.Menu;
 import android.view.MenuItem;
 
 public class IntervalTrainer extends Activity
 {
     ...other stuff...
 
     @Override
     public boolean onCreateOptionsMenu(Menu menu) {
         MenuInflater inflater = getMenuInflater();
         inflater.inflate(R.menu.foo, menu);
         return true;
     }
 
     @Override
     public boolean onOptionsItemSelected(MenuItem item) {
         Intent intent;
         switch (item.getItemId()) {
             case R.id.settings_menu_item:
                 intent = new Intent(this,
                                     DurationPicker.class);
                 intent.putExtra("sprintDuration", sprintDuration);
                 intent.putExtra("restDuration", restDuration);
                 intent.putExtra("numSprints", numSprints);
                 startActivityForResult(intent, PICK_DURATION_REQUEST);
                 return true;
             case R.id.eula_menu_item:
                 showEula();
                 return true;
             case R.id.info_menu_item:
                 showAbout();
                 return true;
             default:
                 return super.onOptionsItemSelected(item);
         }
     }
 }

Activities and Intents
----------------------

Sometimes you are in one screen doing one thing, and you want to switch to
another screen to do something else. For instance you might want to select
the duration in minutes for your exercise, using the DurationPicker class
defined in the IntervalTrainingTimer app. In AndroidManifest.xml, we define
the DurationPicker activity within the application tag::

    <activity android:name="DurationPicker"
              android:screenOrientation="portrait"
              android:label="@string/app_name">
      <intent-filter>
        <action android:name="android.intent.action.PICK" />
        <category android:name="android.intent.category.DEFAULT" />
      </intent-filter>
    </activity>

The main class can kick off the picker from a menu as described earlier.
I believe it could also be started with a button or other control. In any
case the code involved looks like this::

    private static final int PICK_DURATION_REQUEST = 0;
  
    intent = new Intent(this,
                        DurationPicker.class);
    intent.putExtra("sprintDuration", sprintDuration);
    intent.putExtra("restDuration", restDuration);
    intent.putExtra("numSprints", numSprints);
    startActivityForResult(intent, PICK_DURATION_REQUEST);

The DurationPicker class extends Activity. Its GUI is specified in
res/layout/number_picker.xml, which works just like the normal layout
XML file. The GUI gets set up in the OnCreate method::

    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.number_picker);
        ((Button)findViewById(R.id.sprint_increment))
            .setOnTouchListener(....);
        ((Button)findViewById(R.id.sprint_decrement))
            .setOnTouchListener(....);
	....
    }

When the picker has a new result, it calls the setResult method on the intent
that kicked it off::

    private void setIntentDuration(String name, int value) {
        Intent intent = getIntent();
        intent.putExtra(name, value);
        setResult(RESULT_OK, intent);
        updateTimes();
    }

The new result is sent back to the main activity::

    protected void onActivityResult(int requestCode, int resultCode,
                                    Intent data) {
        if (requestCode == PICK_DURATION_REQUEST) {
            if (resultCode == RESULT_OK) {
                sprintDuration = data.getExtras().getInt("sprintDuration");
                restDuration = data.getExtras().getInt("restDuration");
                numSprints = data.getExtras().getInt("numSprints");
            }
        }
    }

Sending a result to the main activity *does not* change the screen. You stay
in the duration picker screen until you hit the BACK key. This allows you to
specify several different results in the picker screen before going back.

Threads and UI stuff
--------------------

In Android, there is a main thread that handles all UI interactions, and
`worker threads`_ can run in the background to handle slow tasks. Don't put
slow tasks on the UI thread, it makes your app behave sluggishly.

A `Handler`_ allows you to send and process Message and Runnable objects
associated with a thread's MessageQueue. Each Handler instance is associated
with a single thread and that thread's message queue. When you create a new
Handler, it is bound to the thread and associated message queue that is
creating it -- from that point on, it will deliver messages and runnables to
that message queue and execute them as they come out of the message queue.
There are two main uses for a Handler: (1) to schedule messages and runnables
to be executed at some point in the future; and (2) to enqueue an action to be
performed on a different thread than your own.

Scheduling messages is accomplished with the *post(Runnable)*,
*postAtTime(Runnable, long)*, *postDelayed(Runnable, long)*,
*sendEmptyMessage(int)*, *sendMessage(Message)*, *sendMessageAtTime(Message,
long)*, and *sendMessageDelayed(Message, long)* methods. The *post* versions
allow you to enqueue Runnable objects to be called by the message queue when
they are received; the *sendMessage* versions allow you to enqueue a Message
object containing a bundle of data that will be processed by the Handler's
handleMessage(Message) method (requiring that you implement a subclass of
Handler).

.. _`worker threads`: http://developer.android.com/guide/topics/fundamentals/processes-and-threads.html#WorkerThreads
.. _`Handler`: http://developer.android.com/reference/android/os/Handler.html

Logging and "adb logcat"
------------------------

TODO

Unit tests and test-driven development
--------------------------------------

TODO

Magic Google APIs (like Google Maps)
------------------------------------

TODO

2D and 3D Graphics
------------------

TODO

Tablets and UI fragments
------------------------

TODO

SQL Database
------------

TODO

Persistent Settings
-------------------

There is a `SharedPreferences`_ class that can be used for this. You'd
probably want some kind of a Picker class for controlling the settings.

.. _`SharedPreferences`: http://developer.android.com/reference/android/content/SharedPreferences.html

TODO: more...


What am I forgetting?
---------------------

There's everything about logging, and "adb logcat", and debugging. There's
unit tests and test-driven development. There's all those magical APIs like
the Google Maps API. Then there's new stuff in Android 3.0 to support tablets,
like UI fragments.

I'm mystified about the "action" piece of the `Intent Filter`_ specified in
AndroidManifest.xml. No rigorous documentation appears to exist for it,
leaving several open questions:

.. _`Intent Filter`: http://developer.android.com/guide/topics/intents/intents-filters.html
