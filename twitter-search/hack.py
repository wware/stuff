#!/usr/bin/python

import getopt
import json
import urllib
import pprint
import os
import sys
import re
import types
import ConfigParser
import smtplib
import mimetypes
from email.MIMEMultipart import MIMEMultipart
from email.MIMEBase import MIMEBase
from email.MIMEText import MIMEText
from email.MIMEAudio import MIMEAudio
from email.MIMEImage import MIMEImage
from email.Encoders import encode_base64

DEBUG = True

# config stuff
CONFIG_FILE = os.path.join(os.environ["HOME"], "twittersearch.ini")

config = ConfigParser.ConfigParser()
config.readfp(open(CONFIG_FILE))

got_urls = set()

class Tweet:
    def __init__(self, dct):
        self.fromUser = dct["from_user"]
        self.fromName = dct["from_user_name"]
        self.text = text = dct["text"]
        self.timestamp = dct["created_at"]
        r = re.compile("http://[^ ]*[^ .]")
        s = set()
        while True:
            m = r.search(text)
            if m is None:
                break
            url, text = text[m.start():m.end()], text[m.end():]
            s.add(url)
        self.urls = list(s)

    def __repr__(self):
        return "<Tweet from " + self.fromUser + ">"

    def html(self, wrapper=None):
        r = ""
        if wrapper is not None:
            r += "<" + wrapper + ">"
        r += "<b>" + self.fromUser + "</b> " + self.text
        htmlurls = [ ]
        # TODO set up a thread pool to read the URLs in bunches
        # of a dozen at a time
        for url in self.urls:
            if url in got_urls:
                continue
            got_urls.add(url)
            try:
                inf = urllib.urlopen(url)
            except:
                continue
            R = inf.read()
            # TODO maintain a set of MD5s for web page contents,
            # discard anything that matches a previous md5sum
            title = ""
            try:
                m = R.index("<title>")
                n = R.index("</title>")
                title = R[m+7:n].strip()
            except ValueError:
                pass
            inf.close()
            if title == "" or \
                    title == "Twitter / ?" or \
                    title == "403 - Forbidden" or \
                    title == "404 - Document Not Found":
                continue
            if DEBUG:
                print url
            try:
                htmlurls.append("<li><a href=\"%s\">%s</a></li>" %
                                (url, title))
            except:
                pass
        if htmlurls:
            r += "<ul>" + ("".join(htmlurls)) + "</ul>"
        if wrapper is not None:
            r += "</" + wrapper + ">"
        return r

def performTwitterSearch(searchterm):                         
    # https://dev.twitter.com/docs/using-search
    URLBEGIN =  "https://search.twitter.com/search.json"
    url = URLBEGIN + "?q=" + urllib.quote(searchterm)
    inf = urllib.urlopen(url)

    R = inf.read()
    inf.close()

    stuff = json.loads(R)
    try:
        results = map(Tweet, stuff["results"])
    except:
        return ""
    if DEBUG:
        for t in results:
            print t

    while len(stuff["results"]) >= stuff["results_per_page"]:
        # get another page
        url = URLBEGIN + stuff["next_page"]
        inf = urllib.urlopen(url)

        R = inf.read()
        inf.close()

        stuff = json.loads(R)
        more = map(Tweet, stuff["results"])
        if DEBUG:
            for t in more:
                print t
        results += more

    result = "<h1>" + searchterm + "</h1><ul>"
    for tweet in results:
        result += tweet.html("li")
    return result + "</ul>"

def writeFile(text, filename):
    open(filename, "w").write(text)

def sendHtmlMail(recipient, subject, html):
    smtpUser = config.get("SMTP", "USER")
    smtpPassword = config.get("SMTP", "PASSWORD")
    smtpServer = config.get("SMTP", "SERVER")
    smtpPort = config.get("SMTP", "PORT")
    msg = MIMEMultipart()
    msg['From'] = smtpUser
    msg['To'] = recipient
    msg['Subject'] = subject
    msg.attach(MIMEText(html, "html"))
    mailServer = smtplib.SMTP(smtpServer)
    mailServer.ehlo()
    mailServer.login(smtpUser, smtpPassword)
    mailServer.sendmail(smtpUser, recipient, msg.as_string())
    mailServer.close()
    print('Sent email to %s' % recipient)

def main(recipient, searchterms):
    # getopt: control debug
    # getopt: choose a different INI file
    title = "Twitter search: " + (" ".join(searchterms))
    html = ""
    for term in searchterms:
        html += performTwitterSearch(term)
    html = html.encode("utf8")
    open("/tmp/twitter-search.html", "w").write(html)
    sendHtmlMail(recipient, title, html)

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2:])
