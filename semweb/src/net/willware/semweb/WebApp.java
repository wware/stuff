package net.willware.semweb;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

// http://www.apl.jhu.edu/~hall/java/Servlet-Tutorial/index.html

public class WebApp extends HttpServlet {
	private static final long serialVersionUID = 1L;
	public void doGet(HttpServletRequest request, HttpServletResponse response)
	throws ServletException, IOException {
		// Use "request" to read incoming HTTP headers (e.g. cookies)
		// and HTML form data (e.g. data the user entered and submitted)
		if (true) throw new RuntimeException("Ouch");

		// Use "response" to specify the HTTP response line and headers
		// (e.g. specifying the content type, setting cookies).

		PrintWriter out = response.getWriter();
		// Use "out" to send content to browser
		out.println("Hello world");
	}
}
