Introduction to Django
======================

Django is a web application framework written in Python. It has a lot
in common with other web app frameworks like Rails, Grails, Spring,
and others. Django has:

* A model-view-controller pattern
* Object-relational mapping for database access
* URL mapping
* HTML templating
* Caching configuration
* AJAX support
* User authentication
* Lots of libraries and modules for different purposes

Django has `good docs <http://docs.djangoproject.com/en/1.2/>`_ and a
good tutorial.

Installing Django on Linux is easy::

 sudo apt-get install python-setuptools
 sudo easy_install django

Python is an interpreted, object-oriented language. Variables are not
typed but values are. Classes and instances, but no notion of a
Java-like compile-time interface. Python emphasizes readability and
user friendliness, and has a large varied collection of libraries.
There is an official `Python website <http://www.python.org>`_.

When a Django web server receives an HTTP request, it matches the URL
to one of several regular expressions in a Python source file called
urls.py, which specifies a handler for each regular expression. The
first matching handler is passed an HTTP request object. Handlers are
called "views" and typically live in a file called views.py.

"Models" are Python classes which represent database tables. Each
column of the DB table corresponds to a settable/gettable property of
the model.

Django has a development server. When you use it, your website will
have a URL like `http://localhost:8000/ <http://localhost:8000/>`_.

HTML templating
---------------

Django has a great templating system. Here's an example::

 <form id="login" class="clearfix" action="/calendar/" method="post" name="signup">
   {% for hidden in hiddeninputs %}
   <INPUT TYPE="HIDDEN" NAME="{{ hidden.name }}" VALUE="{{ hidden.value }}"/>
   {% endfor %}
   <label for="username">Choose an existing location</label>
   <select name="id" value="{{ location.id }}">
     <option value="" selected>
       Choose an existing location...
     </option>
     {% for location in locations %}
     <option value="{{ location.id }}">
       {{ location.city }}, {{ location.state }} &mdash;
       {{ location.name }}, {{ location.street }}
     </option>
     {% endfor %}
   </select>
 </form>

There are for-loop constructs {% for x in list %} ... {% endfor %}

There are conditionals {% if condition %} ... {% endif %}

There are text replacements {{ include_this }}

We can dot something in a template, to access a property in Python,
or to pull a value out of a key-value dictionary. So we can pass
very JSON-like data into the template, and this is helpful for
testing and as an abstraction layer between front end and back end.

Templates can inherit from other templates. Here is a base template::

 <!DOCTYPE html PUBLIC "-//WAPFORUM//DTD XHTML Mobile 1.0//EN" "http://www.wapforum.org/DTD/xhtml-mobile10.dtd">
 <html xmlns="http://www.w3.org/1999/xhtml">
   <head>
     ...meta stuff, stylesheets, other CSS...
   </head>
   <body>
     <div class="container">
       {% block top %}
       <div id="header">
         <a href="/event/" class="home"><img src="/media/images/add.gif" alt="add" /></a>
         <!--<a href="/" class="logo">-->
           <img src="/media/images/logo.gif" alt="Scrunchr" />
         <!--</a>-->
         <a href="/gcalsync/" class="home"><img src="/media/images/side.gif" alt="syn" /></a>
       </div><!-- header -->
       {% endblock %}<!-- top -->
       {% block contents %}
       <div id="banner">
       ...blah blah blah...
       </div>
       {% endblock %}
     </div>
     ...blah blah blah...
   </body>
 </html>

The inherting template can be very simple::

 {% extends "login.html" %}
 {% block contents %}
 <h2>There is a problem here</h2>
 {{ problem|safe }}
 {% endblock %}

The contents of "block contents" are replaced with the content here.


Django tidbits
--------------

* Django scales well if you use POST to change data, and GET only to
  query data.
* Django can use authentication services provided by Facebook,
  Twitter, etc.
* Django tries to adhere to a "Don't Repeat Yourself" philosophy
* Django works with several databases, and mostly insulates you from
  the differences between them: MySQL, Postgresql, SQLite3, Oracle.
* Django's OR M system has query operations that do SQL JOINs, queries
  spanning multiple tables.
* Django's templating system includes a template hierarchy, like a
  class hierarchy, so you can do most of your front-end design work
  near the root of the hierarchy tree.

URL shortener
-------------

When I gave this talk to some folks at my brother's company, they challenged
me to whip up a URL shortener on the spot. It was a lunchtime talk and I knew
I didn't have time to do it then, but I've had time since. So at the bottom of
the front page, you'll find a simple URL shortener with some example usages.
