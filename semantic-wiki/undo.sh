#!/bin/bash

apt-get remove -y apache2 php5 php5-mysql \
    git vim openjdk-6-jdk ant unzip expect \
    php-apc imagemagick php5-intl git ruby mysql-server
apt-get autoremove -y

rm -rf /etc/mediawiki /var/www/wiki
rm -f .setup-step-*-complete
