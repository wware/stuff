Hacking virtualenv for websites
===============================

At work we are using virtualenv to create a sandbox Python environment for
a website. It seems kinda cool and something I should learn more about. My
plan for this directory is to develop a bunch of very small trivial websites
with random combinations of technology stack stuff. All this stuff will be
in Python.

  * Django and sqlite3 (already started)
  * Django, MongoDB, MongoEngine
  * Flask and sqlite3
  * Flask, MongoDB, MongoEngine
  * Flask and Facebook authentication

My hope is to orthogonalize pieces so that it's relatively easy to check
off things on a checklist (Django vs Flask, MongoEngine vs MySQL, etc) and
press a button, and get a trivial example website. Then you'd be able to
focus on the interesting stuff. Over time, there might be APIs for web
services like Google Maps or other such things.

Django and postgresql
---------------------

I'm doing this on a Mac, which offers challenges. I've installed everything
I need using MacPorts.

 sudo port install py27-psycopg2 postgresql92-server findutils
 export PATH=$PATH:/opt/local/lib/postgresql92/bin

Start the server in a dedicated terminal window with

 mkdir -p pg
 postgres -D pg

where pg is a directory for that purpose.

Type "created dbpg" to create a database named "dbpg". Then you can run

 ./hack-virtualenv.sh django-postgresql
