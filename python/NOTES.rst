Notes about Haystack and Whoosh
===============================

I'm using Haystack with Whoosh at work. Whoosh is a breeze but Haystack has dozens and dozens
of moving parts. The first place to look is the `tutorial`_. Using pip to install the
django-haystack package currently gives you the 1.2.7 version, and if you load it straight
from Git or Mercurial or whatever, you get 2.x something, which is wrong.

..  _`tutorial`: http://django-haystack.readthedocs.org/en/v1.2.7/tutorial.html

The first confusing thing is the "search_site" stuff that goes with the HAYSTACK_SITECONF
setting. Here's what I did in our product. I have a file, rue/SM/search_conf.py, with these
contents::

 import haystack
 haystack.autodiscover()

and then in my settings file I have::

 HAYSTACK_SITECONF = 'rue.SM.search_conf'
 HAYSTACK_SEARCH_ENGINE = 'whoosh'
 HAYSTACK_WHOOSH_PATH = os.path.join(os.path.dirname(__file__), 'whoosh_index')

When you put::

 (r'^search/', include('haystack.urls')),

in your urls.py file, magic happens. You'll want to set up a `template for your view`_ but
otherwise Haystack will take care of all the view stuff.

.. _`template for your view`: http://django-haystack.readthedocs.org/en/v1.2.7/tutorial.html#search-template

Steps you need to do to get everything set up
---------------------------------------------

syncdb -- I think you don't need to worry about this, unless you've simultaneously changed your
Django schema. But then you'd need to do it anyway.

update_index, rebuild_index -- these are management commands that appear when you add "haystack"
to your INSTALLED_APPS. Before you can use Haystack, you'll want to run "rebuild_index". I use it
in the setUp methods of my unit tests, like this::

 from django.core.management import call_command
 call_command('rebuild_index', interactive=False, verbosity=0)

Getting search templates working
--------------------------------

The search template is **not** the same as the viewing template. The purpose of the search
template is to take the fields you want to use in your search index and render them as text so
that Haystack's text-searching engine can dig through them. A typical example looks like this::

 {{ object.title }}
 {{ object.user.get_full_name }}
 {{ object.body }}

The location of the search templates is relativew to one of the TEMPLATE_DIRS specified in your
Django settings file. Some of the files we are using, relative to the TEMPLATE_DIRS entry, are these::

 search/indexes/brands/brand_text.txt
 search/indexes/products/product_text.txt

The "brands" and "products" directories are named after apps on our website. The filenames
"brand_text.txt" and "product_text.txt" refer to database models (Brand, Product) where the "text"
field in the corresponding SearchIndex has these magical properties::

 text = haystack.fields.CharField(document=True, use_template=True)

The potentially tricky part is figuring out what Haystack considers to be the name of the app.
If you set it up right, you can get an error message complaining that it can't find the template
file, and then it will mention where it expects to find it, including the app name.

Random Django notes
===================

In my first month at Rue La La, I've learned more about Django than I did working on a six-month
project in the past. (In that case, I had the luxury of clinging fiercely to my comfort zone.)

Related fields and many-to-many relationships
---------------------------------------------

Bearing in mind that schemas describe tables in relational databases, the way we represent a
one-to-many relationship is with a ForeignKey from the "many" thing to the "one" thing. So if
a sky has many stars in it, we have something like this::

 class Sky(models.Model):
     color = models.CharField(max_length=20)
 
 class Star(models.Model):
     name = models.CharField(max_length=20)
     sky = models.ForeignKey(Sky, related_name='stars')
     # if we needed to define Sky later, we could use the string 'Sky' as the first argument of the
     # ForeignKey constructor in order to avoid a forward reference

Notice that "related" name thing. That means if you have an instance of Sky, for instance the sky
as seen from Montana, you can get a list of the stars in the sky by writing::

 [star for star in montanaSky.stars.all()]

When you say "montanaSky.stars", you get a `Manager`_ which you can use to `retrieve things`_.

.. _`Manager`: https://docs.djangoproject.com/en/dev/topics/db/managers/
.. _`retrieve things`: https://docs.djangoproject.com/en/dev/topics/db/queries/#retrieving-objects

For a many-to-many relationship, we typically select one of the two things as the more fundamental
thing. Pizzas and toppings have a many-to-many relationship but we rarely start with the topping
and ask for a list of pizzas. We usually start with a pizza and ask for the list of toppings. So
the pizza is more fundamental::

 class Topping(models.Model):
     name = models.CharField(max_length=20)
 
 class Pizza(models.Model):
     toppings = models.ManyToManyField(Topping, through='PizzaToppingMapping')
 
 class PizzaToppingMapping(models.Model):
     ...fields...

Specifying a "through" model is optional. If we didn't, the ORM layer would automatically create a
table with a column for the pizza ID and a column for the topping ID. When we specify "through", we
make that explicit, and then we can add more fields to the connection.
