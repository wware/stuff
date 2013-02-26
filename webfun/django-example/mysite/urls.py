from django.conf.urls.defaults import *

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
    (r'^$', 'mysite.polls.views.index'),
    (r'^polls/$', 'mysite.polls.views.polls'),
    (r'^code/(?P<filename>\S+)$', 'mysite.polls.views.showcode'),
    (r'^polls/(?P<poll_id>\d+)/$', 'mysite.polls.views.detail'),
    (r'^polls/(?P<poll_id>\d+)/results/$', 'mysite.polls.views.results'),
    (r'^polls/(?P<poll_id>\d+)/vote/$', 'mysite.polls.views.vote'),
    # URL shortener
    (r'^createShortUrl/$', 'mysite.polls.views.newShortUrl'),
    (r'^u/(?P<code>[A-Za-z0-9]+)', 'mysite.polls.views.lookupShortUrl'),
    # admin stuff
    (r'^admin/', include(admin.site.urls)),
)
