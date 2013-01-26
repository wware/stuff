#!/bin/bash

if cat /proc/cpuinfo | grep ' lm ' > /dev/null
then
    CPU_BITS=64
else
    CPU_BITS=32
fi

IPADDR=$(/sbin/ifconfig | grep 'inet addr' | grep -v '127.0.0' | cut -c 21-35 | sed 's/ //g')

if [ ! -f ~/.setup-step-one-complete ]
then

    if [ ! -f home-dot-ssh.tgz ]
    then
	echo "Can't continue without home-dot-ssh.tgz"
	exit 1
    fi

    tar xfz home-dot-ssh.tgz
    rm home-dot-ssh.tgz

    sudo apt-get update
    sudo apt-get install git vim openjdk-6-jdk groovy ant
    # sudo apt-get install jenkins python-jenkins ? ? ?

    for x in stuff private
    do
	git clone git@github.com:wware/$x.git
    done

    wget http://dist.springframework.org.s3.amazonaws.com/release/GRAILS/grails-2.2.0.zip
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
export PATH=$$PATH:$$JAVA_HOME/bin:$$GROOVY_HOME/bin:$$GRAILS_HOME/bin
EOF
    echo "Recommended: source ~/.bashrc"

    wget http://dumps.wikimedia.org/mediawiki/1.20/mediawiki-1.20.2.tar.gz
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

elif [ ! -f ~/.setup-step-two-complete ]
then

    if [ ! -f rdf-cgi.py ]
    then
        echo "We need rdf-cgi.py to continue"
        exit 1
    fi

    a2enmod rewrite
    head -2 /etc/apache2/sites-enabled/000-default > /tmp/000-default
    cat >> /tmp/000-default <<EOF
	RewriteEngine on
	RewriteRule ^/rdf/([A-Z][_a-zA-Z0-9]*)$  /cgi-bin/rdf-cgi.py?title=$1  [PT]
EOF
    tail --line=+3 /etc/apache2/sites-enabled/000-default >> /tmp/000-default
    mv -f /tmp/000-default /etc/apache2/sites-enabled

    sudo service apache2 restart

    sudo mv rdf-cgi.py /usr/lib/cgi-bin
    sudo chmod 755 /usr/lib/cgi-bin/rdf-cgi.py

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

    touch ~/.setup-step-two-complete
    exit 0

else

    echo "You should be all set"

fi
