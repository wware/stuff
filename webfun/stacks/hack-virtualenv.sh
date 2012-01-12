#!/bin/bash

#ENVIRONMENT=django-sqlite3
ENVIRONMENT=$1
DJANGOPROJECT=trivialproject

virtualenv --no-site-packages ${ENVIRONMENT}
cd ${ENVIRONMENT}
source bin/activate

pip install -r ../requirements.txt || exit 1

# Tweak the settings file, but keep the secret key
N=$(grep -n SECRET_KEY ../settings.py | sed 's/:.*//')
head -$((N-1)) ../settings-${ENVIRONMENT}.py > ${DJANGOPROJECT}/settings.py
python ../genkey.py >> ${DJANGOPROJECT}/settings.py
tail --lines=+$((N+1)) ../settings-${ENVIRONMENT}.py >> ${DJANGOPROJECT}/settings.py

cd ${DJANGOPROJECT}

python manage.py syncdb

python manage.py test || exit 1

python manage.py runserver

cd ..
pip freeze > ../requirements.txt

deactivate 
