<html>
<head><title>Here is my web app</title></head>
<body>
<h1>My Servlet</h1>
Here it is.
The time is now <%= new java.util.Date() %>
<p>

<pre>
<!-- % net.willware.semweb.Foo.doStuff(new java.io.PrintStream(new net.willware.semweb.WriterOutputStream(out))); % -->
<% net.willware.semweb.Foo.doStuff(out); %>
</pre>
</body>
</html>
