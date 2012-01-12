<?php
    if ($_REQUEST['ask'] == '') {

# This first part is where we send out a HTML page which the client will use to
# make requests. The requests will come back to this same PHP page, but the 'ask'
# part of the REQUEST won't be empty this time, so then we move on to the next
# part where we send back a cheery little text string with the time. The Javascript
# embedded in the HTML page will accept that string and stick it into the html div
# whose identifier is "result".

?>
<HTML>
      <head>
            <title>AJAX with PHP & Javascript</title>
      </head>
      <body>
          Silly little AJAX demo
          <table>
          <tr><td bgcolor="lightblue">
                 <button type="button" onclick="onclickfunc();">Click</button> to send a request
          to the server
          </td></tr>
          <tr><td bgcolor="yellow">
                 <div id="result">Empty so far</div>
          </td></tr>
          </table>
      </body>
      <script type="text/javascript">
      // The browser might cache the results of HTTP requests, so we need an incrementing
      // count to make sure each URL is unique enough to really fire off a new request.
      var count = 0;
      function onclickfunc()
      {
          var xmlhttp;
          if (window.XMLHttpRequest) {
              // IE7+, Firefox, Chrome, Opera, Safari
              xmlhttp = new XMLHttpRequest();
          } else if (window.ActiveXObject) {
              // IE6, IE5
              xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
          } else {
              alert("Your browser does not support XMLHTTP!");
          }
          // Handler for the server's response
          xmlhttp.onreadystatechange = function() {
              if (xmlhttp.readyState == 4) {
                  document.getElementById('result').innerHTML = xmlhttp.responseText;
              }
          }
          xmlhttp.open("GET", "blarg.php?ask=foo&count=" + count, true);
          count++;
          xmlhttp.send(null);
      }
      </script>
</HTML>

<?php
} else {

# Here's our happy little response to the AJAX request, where we send back a little
# string with the time. We could put anything in here. In many cases people send back
# XML at this point, but plain text works as well.

?>
Yummy AJAX stuff from the server at
<?php
   echo date('M, d Y (H:i:s)');
}
?>
