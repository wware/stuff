JQuery Mobile example project
=============================

JQuery Mobile appears to be a big thing for Android development that I should learn about.

This is taken from http://mobile.tutsplus.com/tutorials/mobile-web-apps/jquery_android/

I've updated the project and gotten it running from the command line. It wouldn't run from
Eclipse until I reinstalled the 2.3.3 target, which apparently I had previously installed
incorrectly, but Eclipse is now working.

I still need to actually read the three-part article. The gist as I understand it is that
you develop an "app" in terms of HTML/CSS/Javascript, doing everything that doesn't require
actual platform stuff, which can all be done with Google Chrome, and when you want to make
an actual app out of it, you give it a thin wrapper of Java code. You can also add stuff in
the Java code, like access to location services or Bluetooth or whatever. Meanwhile all
your front-end work is in HTML/CSS/JS which is probably easier than doing a UI in Android
terms. At least you can farm it out to standard web front end monkeys.

I've noticed that while running the app on a Kindle Fire, if I set up some categories they
work fine, but if I leave the app and re-enter it, it's broken. It was intended as a quick
tutorial demo, not a bullet-proof production-ready app.

Running the UI in a browser
---------------------------

Theoretically the UI can operate in a browser because it's HTML/CSS/Javascript. And it does,
except for one piece that's a PHP file called bridge.php. Even if I configure Apache to run
PHP (see https://help.ubuntu.com/8.04/serverguide/C/php5.html) it still doesn't work. I
needed to

* set DocumentRoot to
  /home/wware/stuff/android_apps/JQueryMobileExample/news-web-code in
  /etc/apache2/sites-available/default
* install the php5-curl package
* in bridge.php, changed tmpFile from 'tmpFile.txt' to '/tmp/tmpFile.txt' so that PHP would
  have sufficient permissions to write a temporary text file there

But still, no dice. Weird because the Android app works fine in the emulator. I tried
putting some debug statements in the PHP to see what's up, still no dice. Nothing looks
incorrect in the PHP code as far as I can tell. After some experiments I found I could use
bridge.php in Google Chrome to read a web page off my VPS, but the web page had to be
absolutely flawless XHTML.

These are the differences between the web code and the Android code.

* The web code uses bridge.php, the Android code does not.
* In the JS in index.html, the NEWS_URI variable is 'bridge.php?fwd=http://rss.news.yahoo.com/rss/'
  in the web code but 'http://rss.news.yahoo.com/rss/' in the Android code.
* The JS in the Android code defines a function changeLocation. In the Android Java code, the
  WebViewClient's shouldOverrideUrlLoading method calls this JS function.