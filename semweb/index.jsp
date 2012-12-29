<html>
<head><title>Here is my web app</title></head>
<body>

<%@ page import="java.text.SimpleDateFormat" %>
<%@ page import="net.willware.semweb.Foo" %>

<h1>My Servlet</h1>
This is a JSP so it can tell you that the time is now <%= new java.util.Date() %>
<p>
It took some monkeying around to get JSP, servlets, and Tomcat all to work together nicely, but now
they're doing it. It appears that a servlet is a non-starter if it doesn't have an /index.html or
/index.jsp file. Also you may need to fiddle with firewalls a little bit.
<p>
This war file is now working on two different Tomcat servers, one on my laptop running Ubuntu 10.04,
and the other on a VBox instance of Ubuntu 12.04 server which was set up with a pretty minimal environment.
So I feel like I've learned something today.

<pre>
<% Foo foo = new Foo(); foo.doStuff(request.getHeader("host"), out); %>
</pre>
</body>
</html>
