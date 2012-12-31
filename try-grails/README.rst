Hacking Grails
==============

Rich and Philippe have both suggested trying Grails. Here we go.

* http://www.youtube.com/watch?v=8d1hp8n1stA  - 90-minute video tutorial
* http://grails.org/tutorials
* http://www.vogella.com/articles/Grails/article.html
* http://grails.org/Quick%20Start

Preparations:

* sudo apt-get install openjdk-6-jdk groovy
* Unpack grails-2.2.0.zip in the home directory, or in /opt.
* In ~/.bashrc, add JAVA_HOME and GRAILS_HOME, and add $GRAILS_HOME/bin to PATH.
* . ~/.bashrc
* Start working thru Quick%20Start.

If you've been fooling with Tomcat separately, type "sudo service tomcat6 stop" so that
you don't block Grails from running its own Tomcat server.

If you see something like "Error starting Sun’s native2ascii", that might mean you've
set JAVA_PATH incorrectly. It should not be set for the JRE, it should be set for the
JVM where bin/javac is.

When the tutorial app is running, you should be able to type::

 (cd my-project; grails run-app)

and then visit http://localhost:8080/my-project/book/list in the browser to see the app
maintaining a list of books.

On the VM, prepare:
sudo apt-get install openjdk-6-jdk groovys
Unpack grails-2.2.0.zip in the home directory, or in /opt.
Add JAVA_HOME and GRAILS_HOME to .bashrc, and add $GRAILS_HOME/bin to PATH. Type ". ~/.bashrc".
Then you can start working thru Quick%20Start.

If you've been fooling with Tomcat separately, type "sudo service tomcat6 stop" so that
you don't block Grails from running its own Tomcat server.

Oops! Works fine on Ubuntu 12.04 so far, but problems on Ubuntu 10.04::

 | Running Grails application
 | Error 2012-12-30 20:44:32,049 [localhost-startStop-1] ERROR plugins.DefaultGrailsPluginManager  - Error configuring dynamic methods for plugin [controllers:2.2.0]: String index out of range: 1
 Message: String index out of range: 1
     Line | Method
 ->> 1946 | substring                     in java.lang.String
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 |    224 | getPropertyNameRepresentation in grails.util.GrailsNameUtils
 |    334 | innerRun . . . . . . . . . .  in java.util.concurrent.FutureTask$Sync
 |    166 | run                           in java.util.concurrent.FutureTask
 |   1110 | runWorker . . . . . . . . . . in java.util.concurrent.ThreadPoolExecutor
 |    603 | run                           in java.util.concurrent.ThreadPoolExecutor$Worker
 ^    636 | run . . . . . . . . . . . . . in java.lang.Thread
 | Error 2012-12-30 20:44:35,333 [Thread-10] ERROR plugins.DefaultGrailsPlugin  - Error configuration scaffolding: null
 Message: null
    Line | Method
 ->> 636 | run in java.lang.Thread
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

I see no reason to accommodate obsolete Ubuntu versions. I had another project that I thought
required 10.04 on my laptop, but I was mistaken, so I'm free to switch to 12.04 and run 10.04
for that project in a virtual machine.
