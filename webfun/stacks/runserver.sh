#!/bin/bash

#ENVIRONMENT=django-sqlite3
ENVIRONMENT=$1
DJANGOPROJECT=trivialproject

source ${ENVIRONMENT}/bin/activate

(cd ${ENVIRONMENT}/${DJANGOPROJECT}
 echo $(pwd)
 python manage.py runserver)
