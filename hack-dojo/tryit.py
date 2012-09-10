#!/usr/bin/python

import os.path
import cherrypy

current_dir = os.path.dirname(os.path.abspath(__file__))
static_dir = os.path.join(current_dir, 'static')

class HelloWorld(object):

    @cherrypy.expose
    def index(self):
        r = '<h1>Index of files and stuff</h1><ul>'
        for (path, dirs, files) in os.walk(current_dir):
            r += '<li>' + path
            if files:
                r += '<ul>'
                for f in files:
                    if path == static_dir:
                        f = '<a href="/static/' + f + '">' + f + '</a>'
                    r += '<li>' + f + '</li>'
                r += '</ul>'
            r += '</li>'
        return r + '</li>'

if __name__ == '__main__':
    print 'static_dir = ' + static_dir

    conf = {'/static': {'tools.staticdir.on': True,
                        'tools.staticdir.dir': static_dir}}

    cherrypy.quickstart(HelloWorld(), '/', config=conf)
