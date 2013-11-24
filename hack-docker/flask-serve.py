#!/usr/bin/env python

import docopt
import flask
import sys

app = flask.app.Flask(__name__)

options = docopt.docopt('''\
Serve static files using the Flask micro web framework.
Prerequities:
  sudo apt-get install python-flask python-setuptools
  sudo easy_install pip virtualenv docopt

Usage:
  {0}

Example client stuff:
  curl http://192.168.1.4:5000/foo.tar.gz -o foo.tar.gz
'''.format(sys.argv[0]))

@app.route('/<filename>')
def hello_world(filename):
    # app.logger.info(filename)
    return open(filename).read()

if __name__ == '__main__':
    app.run(host='0.0.0.0')
