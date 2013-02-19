#!/bin/bash

# This is for a Django app with various possible database choices,
# currently including "sqlite3" and "postgresql".

WHICHDB=$1
DJANGOPROJECT=trivialproject

virtualenv --no-site-packages django-app
(
cd django-app
source bin/activate

pip install -r ../requirements-${WHICHDB}.txt || exit 1

# Tweak the settings file, but keep the secret key
(
M=$(wc ../settings-django-${WHICHDB}.py | cut -c -8)
N=$(grep -n SECRET_KEY ../settings-django-${WHICHDB}.py | sed 's/:.*//')
head -$((N-1)) ../settings-django-${WHICHDB}.py
python ../genkey.py
tail -n $((M-N)) ../settings-django-${WHICHDB}.py
) > ${DJANGOPROJECT}/settings.py

cd ${DJANGOPROJECT}

python manage.py syncdb

python manage.py test || exit 1

python manage.py runserver

deactivate 
)
