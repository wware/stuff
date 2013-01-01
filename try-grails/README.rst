Hacking Grails
==============

Rich and Philippe have both suggested trying Grails. Here we go.

* http://www.youtube.com/watch?v=8d1hp8n1stA  - 90-minute video tutorial
* http://grails.org/tutorials
* http://www.vogella.com/articles/Grails/article.html
* http://grails.org/Quick%20Start

Preparations:

* sudo apt-get install openjdk-6-jdk groovys
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
