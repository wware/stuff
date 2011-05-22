Helper for Mediawiki websites
=============================

This is to help create a Mediawiki instance hosted on an Ubuntu machine. The
initial population of the wiki is done in a virtual machine instance, using
the following Ubuntu packages::

 sudo apt-get install mediawiki apache2 php5 php5-mysql mysql-server \
   mime-support debconf-2.0 php5-cli imagemagick mediawiki-math \
   memcached clamav

I'm assuming MySQL because I'm familiar with it, but if one wanted to use
PostgreSQL instead, you'd make the following replacements: php5-mysql ->
php5-pgsql, mysql-server -> postgresql-contrib.

Potentially useful web pages

* http://meta.wikimedia.org/wiki/Help:Contents
* http://www.mediawiki.org/wiki/Manual:System_administration
* http://www.mediawiki.org/wiki/Manual:Security
* http://www.mediawiki.org/wiki/Manual:Image_Administration

There are some handy extensions worth considering.

* http://www.mediawiki.org/wiki/Extension:MediawikiPlayer for Youtube
* http://www.mediawiki.org/wiki/Extension:Collection, good for creating
  PediaPress books
* http://www.mediawiki.org/wiki/Extension:GraphViz if you have graphviz
  installed
* http://www.mediawiki.org/wiki/Extension:ABC for music
* http://www.mediawiki.org/wiki/Category:All_extensions

A snapshot of a wiki should contain the following.

* a copy of LocalSettings.php
* uploaded files
* extensions
* a mysqldump of the wiki contents
* the logo (http://www.mediawiki.org/wiki/Manual:LocalSettings.php#Logo)

It would be handy to include a script that packages all that stuff into
a tarball, and somehow facilitates redeployment (maybe with a second
script inside the tarball).
