#!/bin/bash

if cat /proc/cpuinfo | grep ' lm ' > /dev/null
then
    CPU_BITS=64
else
    CPU_BITS=32
fi

IPADDR=$(/sbin/ifconfig | grep 'inet addr' | grep -v '127.0.0' | cut -c 21-34 | sed 's/ //g')

#############################################################
#############################################################
#############################################################
if [ ! -f ~/.setup-step-one-complete ]
#############################################################
#############################################################
#############################################################
then

    sudo apt-get update
    sudo apt-get install -y git vim openjdk-6-jdk groovy ant apache2 unzip \
	libapache2-mod-php5 mysql-server mysql-client php-apc imagemagick \
	php5-intl php5-mysqlnd git ruby || exit 1
    # sudo apt-get install jenkins python-jenkins ? ? ?

    for x in stuff private
    do
	git clone git@github.com:wware/$x.git || exit 1
    done

    wget http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-2.2.0.zip || exit 1
    unzip grails-2.2.0.zip
    mv grails-2.2.0 grails

    if [ "$CPU_BITS" == "64" ]
    then
	JAVA_PACKAGE=java-6-openjdk-amd64
    else
	JAVA_PACKAGE=java-6-openjdk-i386
    fi

    cat >> ~/.bashrc <<EOF
export EDITOR=/usr/bin/vim
export JAVA_HOME=/usr/lib/jvm/$JAVA_PACKAGE
export GROOVY_HOME=/usr/share/groovy
export GRAILS_HOME=$HOME/grails
export PATH=\$PATH:\$JAVA_HOME/bin:\$GROOVY_HOME/bin:\$GRAILS_HOME/bin
EOF
    source ~/.bashrc

    wget http://dumps.wikimedia.org/mediawiki/1.20/mediawiki-1.20.2.tar.gz || exit 1
    tar xvzf mediawiki-1.20.2.tar.gz
    sudo mv mediawiki-1.20.2 /etc/mediawiki

    sudo ln -s /etc/mediawiki /var/www/wiki
    sudo service apache2 restart

    cat <<EOF
Now visit http://$IPADDR/wiki/
and follow steps for "set up this wiki" After it takes your name, it will give
you the option to skip the rest of the setup.  DON'T DO THAT. On the following
page, check "Authorized editors only" and set the license to the GNU FDL. Under
Extensions, click "ConfirmEdit", and "Gadgets".  I'm not installing memcached
now, because I don't know much about it, but it might be good.

You'll need to copy LocalSettings.php onto the server, and get it into the
/etc/mediawiki directory, and then you'll be allowed to use the wiki.  After
getting the wiki set up, re-run this script and we'll move on to the next
piece.
EOF

    touch ~/.setup-step-one-complete
    exit 0

#############################################################
#############################################################
#############################################################
elif [ ! -f ~/.setup-step-two-complete ]
#############################################################
#############################################################
#############################################################
then

    sudo chmod 777 /usr/lib/cgi-bin

    cat >> /usr/lib/cgi-bin/rdf-cgi.py <<EOF
#!/usr/bin/python
import cgi
import os
import re
import urllib
print "Content-Type: plain/text"
print
r1 = re.compile("<textarea readonly[^>]*>")
r2 = re.compile("</textarea>")
r3 = re.compile("===? (rdf|RDF) ===?")
r4 = re.compile("title=[A-Z][_a-zA-Z0-9]*")
pagename = "Main_Page"
if os.environ.has_key('QUERY_STRING'):
    qs = os.environ['QUERY_STRING']
    m = r4.search(qs)
    if m is not None:
        pagename = qs[m.start()+6:m.end()]
url = "http://localhost/wiki/index.php?title=" + pagename + "&action=edit"
R = urllib.urlopen(url).read().replace("&lt;", "<")
R = R[r1.search(R).end():r2.search(R).start()].split('\n')
rdfState = 0
for L in R:
    m = r3.search(L)
    if rdfState == 0 and m is not None:
        rdfState = 1
    elif rdfState == 1 and L[:1] == ' ':
        rdfState = 2
        print L[1:]
    elif L[:1] == ' ':
        print L[1:]
    else:
        rdfState = 0
EOF

    cat >> /usr/lib/cgi-bin/update-fuseki.sh <<EOF
#!/bin/bash
TTLFILE=/tmp/file-\$RANDOM.ttl
rm -f \$TTLFILE
for x in \$(mysql -B -u root --password=XXX -e "use my_wiki; select page_title from page;" | tail --line=+2)
do
    wget -O - http://localhost/rdf/\$x 2>/dev/null >> \$TTLFILE
done
export CP=\$(ls /opt/apache-jena/lib/*.jar | python -c \
    'import sys; print ":".join(map(lambda x:x.rstrip(),sys.stdin.readlines()))')
# Turn off fuseki server, and back on, to empty contents
kill -9 \$(ps ax | grep fuseki-server | grep -v grep | cut -c -5)
fuseki-server --update --mem /ds &
sleep 3
s-put http://localhost:3030/ds/data default \$TTLFILE
rm -f \$TTLFILE
echo "Content-Type: plain/text"
echo ""
echo "OK"
EOF

    chmod 755 /usr/lib/cgi-bin/*
    sudo chmod 755 /usr/lib/cgi-bin

    head -2 /etc/apache2/sites-enabled/000-default > /tmp/000-default
    cat >> /tmp/000-default <<EOF
	RewriteEngine on
	RewriteRule ^/rdf/([A-Z][_a-zA-Z0-9]*)$  /cgi-bin/rdf-cgi.py?title=$1  [PT]
EOF
    tail --line=+3 /etc/apache2/sites-enabled/000-default >> /tmp/000-default
    sudo mv -f /tmp/000-default /etc/apache2/sites-enabled

    sudo a2enmod rewrite
    sudo service apache2 restart

#    Might be handy for Mediawiki debugging
#    cat >> /etc/mediawiki/LocalSettings.php <<EOF
#\$wgShowSQLErrors = true;
#EOF

    sudo chmod 777 /opt
    (
    cd /opt
    wget http://www.apache.org/dist/jena/binaries/apache-jena-2.7.4.tar.gz || exit 1
    wget http://www.apache.org/dist/jena/binaries/jena-fuseki-0.2.5-distribution.tar.gz || exit 1
    for x in *.gz; do tar xfz $x; done
    rm *gz               
    mv apache-jena-2.7.4 apache-jena
    mv jena-fuseki-0.2.5 jena-fuseki
    chmod u+x jena-fuseki/s-*
    )
    cat >> ~/.bashrc <<EOF
export FUSEKI_HOME=/opt/jena-fuseki
export PATH=\$PATH:\$FUSEKI_HOME
EOF
    source ~/.bashrc

    #fuseki-server --update --mem /ds &
    #setup 3
    #s-put http://localhost:3030/ds/data default stuff/semweb/family.ttl
    #s-put http://localhost:3030/ds/data default stuff/semweb/wares.ttl
    #kill -9 $!

    touch ~/.setup-step-two-complete
    exit 0

else

    echo "You should be all set"

fi
