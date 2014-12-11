Making graphs on an embedded device
=

Let's say you have an embedded device like a Raspberry Pi or Beaglebone Black and you want to
make graphs of some data you're collecting. Let's suppose this device runs a webserver and you
want these graphs to be available in the web interface. Here are a couple of options.

Generate a graph in Python with [matplotlib](http://matplotlib.org/)
=

The webserver is in Python (e.g. Django) or has access to Python. You can generate a PNG on the
device as a static file, and then make it available in Django's static resources directory.

```python
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

fig = plt.figure()
ax1 = fig.add_subplot(111)
t = np.arange(0.01, 10.0, 0.01)
s1 = np.exp(t)
ax1.plot(t, s1, 'b-')
ax1.set_xlabel('time (s)')
ax1.set_ylabel('exp')

ax2 = ax1.twinx()
s2 = np.sin(2*np.pi*t)
ax2.plot(t, s2, 'r.')
ax2.set_ylabel('sin')
plt.savefig("graph.png")
```

Using pylab, the first four lines reduce to two import statements; the `use('Agg')` is unnecessary.

```python
import numpy as np
import pylab as plt
```

Here are some useful matplotlib links.

* Lots of examples, by [description](http://matplotlib.org/1.3.1/examples/) and [appearance](http://matplotlib.org/gallery.html)
* [Table of contents](http://matplotlib.org/contents.html) for documentation
* [Stackoverflow questions](http://stackoverflow.com/questions/tagged/matplotlib) about matplotlib
* [Summary](http://matplotlib.org/api/pyplot_summary.html) of plotting commands
* [Logarithmic](http://matplotlib.org/examples/pylab_examples/log_demo.html) axes,
  subticks [1](http://stackoverflow.com/questions/4896176/how-to-put-minorticks-on-both-the-sub-plots)
  [2](http://stackoverflow.com/questions/10781077/how-to-disable-the-minor-ticks-of-log-plot-in-matplotlib)
  [3](http://stackoverflow.com/questions/12493809/minor-ticks-on-logarithmic-axis-in-matplotlib)

It turns out that the code I'll be working with already includes matplotlib code, so together
with the inability of D3 to produce decent PNG files (discussed below), that's a compelling
argument for matplotlib.

Generate a graph in the browser with [D3](http://d3js.org)
=

Since the front end will be more powerful than the back end, maybe a D3-ish
approach in the browser is to be preferred. There is a
[wrapper](http://dimplejs.org/examples_index.html) for D3 that looks pretty
good. There is also a
[nice blog](http://www.d3noob.org/2012/12/starting-with-basic-d3-line-graph.html) for
getting up to speed on D3 hacking.

Here's info about setting up [fancy scales](https://github.com/mbostock/d3/wiki/Quantitative-Scales), e.g. for log-linear or log-log graphs, like a Bode plot.
And how to do [subticks](http://bl.ocks.org/GerHobbelt/3605035) and many other axis tricks. Two of the big D3 gurus are
[Mike Bostock](http://bost.ocks.org/mike/) and [Ger Hobbelt](http://bl.ocks.org/GerHobbelt).

Producing a PNG from a D3-drawn graph?
--

Conversion to PNG still requires
[asking the back end](http://d3export.housegordon.org/) to use
[rsvg-convert](http://cgit.openembedded.org/cgit.cgi/openembedded-core/tree/meta/recipes-gnome/librsvg/librsvg_2.32.1.bb?h=dora)
to generate the PNG, but that would only be done if the user needs to keep a
hardcopy of the graph. I'm loath to include librsvg on the platform I'm working
on because it pulls in a boatload of dependencies including qemu-native, which
strikes me as excessive. My researches to try to find an in-browser approach to
PNG generation have all looked like this, and it produces profoundly ugly, useless
results.

```javascript
var svg = $("body svg")[0];
var svgData = new XMLSerializer().serializeToString(svg);
var canvas = document.createElement("canvas");
var ctx = canvas.getContext("2d");
var img = document.createElement("img");
document.body.appendChild(img);
img.width = 1000;
img.height = 700;
img.setAttribute("src", "data:image/svg+xml;base64," + btoa(svgData));
ctx.drawImage( img, 0, 0 );
```

I really need to find out whether users are expecting to get PNG files from this
thing, because if they are, the D3 approach is a non-starter. I spoke to one of
the other engineers and confirmed that producing PNGs is a requirement, so D3
is out.

A couple more random things to consider
=

* http://nbviewer.ipython.org/gist/msund/11349097
* https://mpld3.github.io/quickstart.html
