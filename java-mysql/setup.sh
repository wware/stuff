#!/bin/sh

mysql -u root -e 'drop database javatest;' 2> /dev/null
mysql -u root -e 'create database javatest; use javatest; source testschema.sql;'
