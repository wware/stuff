#!/bin/bash

# This is for a Django app with various possible database choices,
# currently including "sqlite3" and "postgresql".

WHICHDB=$1
PGBIN=/opt/local/lib/postgresql92/bin
DJANGOPROJECT=trivialproject

case $WHICHDB in
  sqlite3)
    ;;
  postgresql)
    ;;
  *)
    echo "Command line arg: sqlite3 or postgresql"
    exit 0
    ;;
esac

if [ "$WHICHDB" == "postgresql" ]
then
    if [ ! -f pw ]
    then
        echo "Put the password in the file 'pw'"
        exit 0
    fi
    PASSWORD=$(head -1 pw | sed 's/ //g')
    sed "s/'password'/\"$PASSWORD\"/" < settings-django-$WHICHDB-1.py > settings-django-$WHICHDB.py
    if [ "$(ps ax | grep '$PGBIN/postgres -D pg' | wc -l | sed 's/ //g')" == "1" ]
    then
        rm -rf pg || exit 1
        mkdir pg || exit 1
        $PGBIN/initdb pg -E utf8
        $PGBIN/postgres -D pg &
        echo
        echo "Use kill -9 $! to kill the PostgreSQL server process"
        echo
        sleep 5
        $PGBIN/createdb -h localhost dbpg || exit 1
    fi
fi

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
    # python manage.py test || exit 1
    python manage.py runserver

    deactivate
)
