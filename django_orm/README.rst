================
Using Django ORM without using the rest of Django
================

The goal here is to do some tinkering with the ORM component of
Django, as it is a handy way to work with a database. Not that I'm
afraid to get my hands dirty with a little SQL, but they package it up
so very nicely.

I want to do more tinkering with JSON methods for Django models.

------------
Preserving data in Django
------------

When you want to save data for later retrieval, either for a backup
or because you need to update the DB schema, the commands look like
this, assuming you have a 'fixtures' directory in place. A 'fixture'
is just a JSON file for saving DB contents::

  % python manage.py dumpdata --indent 4 > fixtures/silly.json

  % python manage.py flush    # empty the database, or change schema

  % python manage.py loaddata silly.json

The fixture file looks like this::

    [
        {
            "pk": 1, 
            "model": "django_orm.location", 
            "fields": {
                "city": "Framingham", 
                "state": "MA", 
                "street": "12 Francine Road"
            }
        }, 
        {
            "pk": 1, 
            "model": "django_orm.person", 
            "fields": {
                "home": 1, 
                "name": "Will"
            }
        }
    ]

------------
Adding toJson() methods to Django models
------------

Obviously this is not required for preserving data, which was part of
my original rationale for developing it. But it's still useful for
other things, like AJAX, and Django templating.

There should probably be a deep version of toJson, where instead of
including just the integer indices of things (like `home_id` in
Person), you actually return the JSON for that thing::

    >>> import pprint
    >>> from django_orm.models import *
    
    >>> pprint.pprint(Person.objects.get(id=1).toJson())
    {'home_id': 1L, 'id': 1L, 'name': u'Will'}
    
    >>> pprint.pprint(Person.objects.get(id=1).toJson(deep=True))
    {'home': {'city': u'Framingham',
              'id': 1L,
              'state': u'MA',
              'street': u'12 Francine Road'},
     'id': 1L,
     'name': u'Will'}
