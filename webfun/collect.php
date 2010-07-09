<?php
# This is a simple MySQL-based opt-in page.
# People enter their email addresses and then hit
# the Submit button. Here's how to set up the table.
#
# mysql> create database collect;
# mysql> use collect;
# mysql> create table collection ( id int primary key auto_increment,
#                                  email varchar(50) not null );
# mysql> describe collection;
# +-------+-------------+------+-----+---------+----------------+
# | Field | Type        | Null | Key | Default | Extra          |
# +-------+-------------+------+-----+---------+----------------+
# | id    | int(11)     | NO   | PRI | NULL    | auto_increment | 
# | email | varchar(50) | NO   |     | NULL    |                | 
# +-------+-------------+------+-----+---------+----------------+
# 2 rows in set (0.00 sec)
#
# In addition to this file, you'll need jquery.js plus any images
# required for your UI.

# set these appropriately
$dbhost = 'localhost';
$dbuser = 'username';
$dbpasswd = 'password';
?>
<HTML>
  <HEAD>
    <TITLE>Email list opt-in</TITLE>
    <SCRIPT SRC="/scripts/jquery.js" TYPE="text/javascript"></SCRIPT>
    <STYLE TYPE="text/css">
      .emailtextbox { color: #888; background: #ff8; }
      .submitbutton { color: #fff; background: #068; }
      .smalltext { font-size: 0.65em; }
      #tabmenu {
          color: #000;
          margin: 12px 0px 0px 0px;
          padding: 0px;
          z-index: 1;
          padding-left: 600px }
      #tabmenu li {
          display: inline;
          list-style-type: none;
          color: #000;
          border: 1px solid black;
          padding: 2px 5px 0px 5px;
          margin: 0;
          text-decoration: none; }
    </STYLE>
  </HEAD>
  <BODY>
    <TABLE>
      <TR ID="vspacer">
      </TR>
      <TR>
        <TD ID="hspacer">
        </TD>
        <TD>
          <UL ID="tabmenu">
            <LI>Hello, world</LI>
          </UL>
          <DIV ID="box" STYLE="width:780px;border:1px solid black;">
            <TABLE>
              <TR HEIGHT="15">
              </TR>
              <TR>
                <!--  100 + 106 + 100 + 374 + 100 = 780 total width
                  -->
                <TD WIDTH="100">
                </TD>
                <TD>
                  <IMG SRC="/media/foo.jpg" WIDTH="106" HEIGHT="102" />
                </TD>
                <TD WIDTH="100">
                </TD>
                <TD>
                  <IMG SRC="/media/bar.jpg" WIDTH="374" HEIGHT="102" />
                </TD>
                <TD WIDTH="100">
                </TD>
              </TR>
              <TR HEIGHT="20">
              </TR>
<?php
#
# This clause dumps the contents of the database.
# * * * * DO NOT USE THIS IN REAL DEPLOYMENT! * * * *
# It is a horrible privacy violation.
# http://12.34.56.78/collect.php?read=1
if ($_GET['read'] == '1') {
?>
        <TR>
          <TD WIDTH="100">
          </TD>
          <TD COLSPAN="3">
<?php
    mysql_connect($dbhost, $dbuser, $dbpasswd);
    $result = mysql_query("SELECT * FROM collect.collection");
    while ($row = mysql_fetch_assoc($result)) {
        echo $row["id"] . ' ';
        echo $row["email"] . '<BR/>';
    }
    mysql_close();
?>
          </TD>
          <TD WIDTH="100">
          </TD>
        </TR>
        <TR HEIGHT="20">
        </TR>

<?php
} else if ($_POST['email'] == '') {
?>
        <TR>
          <TD WIDTH="100">
          </TD>
          <TD COLSPAN="3">
            <P>This is a simple MySQL-based opt-in page.</P>
            <P>People enter their email addresses and then hit
            the Submit button.</P>
          </TD>
          <TD WIDTH="100">
          </TD>
        </TR>
        <TR HEIGHT="20">
        </TR>
        <TR>
          <TD WIDTH="100">
          </TD>
          <TD COLSPAN="3">
            <DIV ALIGN="right">
            <FORM action="/collect.php" method="POST">
              <INPUT NAME="email"
                     CLASS="emailtextbox"
                     BACKGROUND="yellow"
                     TYPE="text"
                     SIZE="25"
                     MAXLENGTH="50"
                     ONBLUR="this.value=this.value||this.defaultValue;this.style.color='#999';this.style.background='#ff8';"
                     ONFOCUS="this.value=''; this.style.color='#000';this.style.background='#ff8';"
                     VALUE="Enter your email address here" />
              <INPUT TYPE="submit"
                     CLASS="submitbutton"
                     VALUE="Submit" />
            </FORM>
            </DIV>
          </TD>
          <TD WIDTH="100">
          </TD>
        </TR>
        <TR HEIGHT="10">
        </TR>

<?php
} else {
    # Make sure it fits an email address regular expression, otherwise
    # we might be vulnerable to an SQL injection attack.
    $email = $_POST["email"];
    if (eregi("^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3})$",
	      $email)) {
	# if we don't already have this email, add it to the mysql table
        mysql_connect($dbhost, $dbuser, $dbpasswd);
        $result = mysql_query("SELECT * FROM collect.collection WHERE email='$email'");
        $gotIt = 0;
        while ($row = mysql_fetch_assoc($result)) {
            $gotIt = 1;
        }
        mysql_close();
        if ($gotIt == 0) {
	    $query = "INSERT INTO collect.collection (email) VALUE ('$email')";
	    mysql_connect($dbhost, $dbuser, $dbpasswd);
	    mysql_query($query);
	    mysql_close();
        }
        mysql_connect($dbhost, $dbuser, $dbpasswd);
        $result = mysql_query("SELECT * FROM collect.collection WHERE email='$email'");
        while ($row = mysql_fetch_assoc($result)) {
            $id = $row["id"];
        }
        mysql_close();
        ?>
        <TR>
          <TD WIDTH="100">
          </TD>
          <TD COLSPAN="3">
            <P>You are number
	     <?php echo $id; ?>
            in the opt-in list. Thanks!
            </P>
          </TD>
          <TD WIDTH="100">
          </TD>
        </TR>
        <TR HEIGHT="20">
        </TR>

<?php
    }
    else {
?>
        <TR>
          <TD WIDTH="100">
          </TD>
          <TD COLSPAN="3">
            <P>Looks like you mis-typed your email. Please hit the BACK button
            and try again.</P>
          </TD>
          <TD WIDTH="100">
          </TD>
        </TR>
        <TR HEIGHT="20">
        </TR>
<?php
   }
}
?>
            </TABLE>
          </DIV>
        </TD>
      </TR>
    <TABLE>
  <SCRIPT TYPE="text/javascript">
    function setSpacerSizes() {
      var myWidth = 0, myHeight = 0;
      if (typeof(window.innerWidth) == 'number') {
        //Non-IE
        myWidth = window.innerWidth;
        myHeight = window.innerHeight;
      } else if (document.documentElement &&
                  (document.documentElement.clientWidth ||
                   document.documentElement.clientHeight)) {
        //IE 6+ in 'standards compliant mode'
        myWidth = document.documentElement.clientWidth;
        myHeight = document.documentElement.clientHeight;
      } else if (document.body &&
                  (document.body.clientWidth ||
                   document.body.clientHeight)) {
        //IE 4 compatible
        myWidth = document.body.clientWidth;
        myHeight = document.body.clientHeight;
      }
      // Width picks up ~16 extra pixels, height ~24, I dunno why
      var wb = $("#box").get(0).style.width;  // "780px"
      wb = wb.substring(0, wb.length-2);      // "780"
      var n = 0.5 * (myWidth - wb) - 16;
      if (n < 0) n = 0;
      $("#hspacer").get(0).style.width = "" + n + "px";
      var hb = $("#box").height() + $("#tabmenu").height();
      n = 0.35 * (myHeight - hb) - 24;
      if (n < 0) n = 0;
      $("#vspacer").height(n);
    }
    $(document).ready(setSpacerSizes);
    $(window).resize(setSpacerSizes);
  </SCRIPT>
  </BODY>
</HTML>
