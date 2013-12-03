#!/usr/bin/env python

import docopt
import flask
import netifaces
import sys

app = flask.app.Flask(__name__)

options = docopt.docopt('''\
Serve static files using the Flask micro web framework.
Prerequities:
  sudo apt-get install -y python-flask python-setuptools python-dev curl
  sudo easy_install pip virtualenv docopt netifaces

Usage:
  {0}

Example client stuff:
  curl http://{1}:5000/foo.tar.gz -o foo.tar.gz
'''.format(sys.argv[0], netifaces.ifaddresses('eth0')[2][0]['addr']))

@app.route('/<filename>')
def hello_world(filename):
    app.logger.info(filename)
    return open(filename).read()

if __name__ == '__main__':
    app.run(host='0.0.0.0')
