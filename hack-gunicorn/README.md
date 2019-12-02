# Learning about Gunicorn, Docker, Ansible, and possibly Terraform

Let's dockerize [this
tutorial](https://www.digitalocean.com/community/tutorials/how-to-serve-flask-applications-with-gunicorn-and-nginx-on-ubuntu-14-04)
in order to help wrap my head around this WSGI stuff.

Let's also think about how Ansible can be applied to Docker, and maybe later
mess around with some Terraform.

## `https://en.wikipedia.org/wiki/Gunicorn`

The Gunicorn "Green Unicorn" (pronounced jee-unicorn) is a Python Web Server
Gateway Interface (WSGI) HTTP server. It is a pre-fork worker model, ported
from Ruby's Unicorn project. The Gunicorn server is broadly compatible with a
number of web frameworks, simply implemented, light on server resources and
fairly fast.

## `https://en.wikipedia.org/wiki/Web_Server_Gateway_Interface`

The Web Server Gateway Interface (WSGI, pronounced whiskey) is a simple calling
convention for web servers to forward requests to web applications or
frameworks written in the Python programming language. The current version of
WSGI, version 1.0.1, is specified in Python Enhancement Proposal (PEP) 3333.

WSGI was originally specified as PEP-333 in 2003. PEP-3333, published in 2010,
updates the specification for Python 3.

### Background

In 2003, Python web frameworks were typically written against only CGI,
FastCGI, `mod_python`, or some other custom API of a specific web server. To
quote PEP 333:

> Python currently boasts a wide variety of web application frameworks, such as
> Zope, Quixote, Webware, SkunkWeb, PSO, and Twisted Web -- to name just a few.
> This wide variety of choices can be a problem for new Python users, because
> generally speaking, their choice of web framework will limit their choice of
> usable web servers, and vice versa... By contrast, although Java has just as
> many web application frameworks available, Java's "servlet" API makes it
> possible for applications written with any Java web application framework to
> run in any web server that supports the servlet API.

WSGI was thus created as an implementation-agnostic interface between web
servers and web applications or frameworks to promote common ground for
portable web application development.

### Specification overview

The WSGI has two sides:
* the server/gateway side. This is often a full web server such as Apache or
  Nginx, or a lightweight application server that can communicate with a
  webserver, such as flup.
* the application/framework side. This is a Python callable, supplied by the
  Python program or framework.

Between the server and the application, there may be one or more WSGI
middleware components, which implement both sides of the API, typically in
Python code.

WSGI does not specify how the Python interpreter should be started, nor how the
application object should be loaded or configured, and different frameworks and
webservers achieve this in different ways.

### WSGI Middleware

A WSGI middleware component is a Python callable that is itself a WSGI
application, but may handle requests by delegating to other WSGI applications.
These applications can themselves be WSGI middleware components.

A middleware component can perform such functions as:

* Routing a request to different application objects based on the target URL,
  after changing the environment variables accordingly.
* Allowing multiple applications or frameworks to run side-by-side in the same
  process
* Load balancing and remote processing, by forwarding requests and responses
  over a network
* Performing content post-processing, such as applying XSLT stylesheets

## Docker stuff

Run

    docker build -t foobar .

to build a docker image with two python files in it. One is a teeny web app, the other
is a gunicorn wrapper for it. Then you can run the docker image with:

    export CONTAINER=$(docker run -p 8000:8000 -d foobar)

At that point you can visit http://localhost:8000 with a web browser to appreciate
your handiwork. Stop your Docker container by typing:

    docker rm -f ${CONTAINER}

### Messing about with Ansible

There are two playbooks, `playbook.yml` and `undo.yml`, which respectively
bring up and tear down a container, and also take care of the docker-build
stuff above. That's pretty nice, except that while it's doing the very slow
build of the docker image, there is no standard output to let you know how
things are going.
