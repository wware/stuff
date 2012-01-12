import simplejson
import pprint
import sys

DEPTH = 1

def stringOkay(x):
    x = x.strip()
    if len(x) <= DEPTH:
        return False
    def charOkay(ch):
        return ch.lower() in 'abcdefghijklmnopqrstuvwxyz0123456789.-_,'
    for y in x:
        if not charOkay(y):
            return False
    return True

suggestions = map(lambda x: x.strip(),
                  filter(stringOkay,
                         open("/usr/share/dict/words").readlines()))

def handle_name(name, d):
    d2 = d
    lname = name.lower()
    for i in range(DEPTH):
        x = lname[i]
        if i < DEPTH - 1:
            if not d2.has_key(x):
                d2[x] = { }
            d2 = d2[x]
        else:
            if not d2.has_key(x):
                d2[x] = [ ]
            d2[x].append(name)

d = { }
map(lambda name: handle_name(name, d),
    suggestions)

#if len(sys.argv) > 1:
#    pprint.pprint(d)
#    sys.exit(0)

jsonstring = simplejson.dumps(d)

# handy thing for debugging javascript:
# alert(JSON.stringify(someVariable));

print '''<HTML>
<HEAD>
<TITLE>Tinker with auto-complete javascript stuff</TITLE>
<SCRIPT type="text/javascript">
  var d = %(jsonstring)s;
  function substringMatch(shorter, longer) {
    var i;
    shorter = shorter.toLowerCase();
    longer = longer.toLowerCase();
    for (i = 0; i < shorter.length; i++)
      if (shorter[i] != longer[i])
        return 0;
    return 1;
  }
  function bar() {
    var value = document.getElementById('searchbox').value;
    if (value == '%(searchphrase)s') {
      document.getElementById('searchbox').value = '';
    }
  }
  function xyzzy() {
    var value = document.getElementById('searchbox').value;
    if (value == '') {
      document.getElementById('searchbox').value = '%(searchphrase)s';
    }
  }
  function foo() {
    var i, j, d2, s;
    var value = document.getElementById('searchbox').value;
    if (value.length == 0) {
      document.getElementById('results').innerHTML = '';
    } else if (value.length >= %(depth)d) {
      d2 = d;
      for (i = 0; i < %(depth)d; i++) {
        d2 = d2[value.substring(i, i+1)];
        if (d2 == null) break;
      }
      if (d2 != null) {
        s = "";
        for (i = j = 0; i < d2.length; i++) {
          if (substringMatch(value, d2[i])) {
            if (j == %(maxsuggestions)d)
              s += "and others...";
            else if (j < %(maxsuggestions)d)
              s += d2[i] + "<BR>";
            j++;
          }
        }
        document.getElementById('results').innerHTML = s;
      }
    }
  }
</SCRIPT>
</HEAD>
<BODY>
<H1>Tinker with auto-complete javascript stuff</H1>
<!-- http://www.w3schools.com/TAGS/tag_input.asp -->
<INPUT TYPE="TEXT"
       ID="searchbox"
       ONKEYUP="foo();"
       ONFOCUS="bar();"
       ONBLUR="xyzzy();"
       VALUE="%(searchphrase)s" />
<DIV ID="results">
</DIV>
</BODY>
</HTML>
''' % {
    'depth': DEPTH,
    'jsonstring': jsonstring,
    'searchphrase': 'Search for words...',
    'maxsuggestions': 40,
    }
