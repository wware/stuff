from django.conf.urls.defaults import *

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
    (r'^$', 'polls.views.index'),
    (r'^polls/$', 'polls.views.polls'),
    (r'^code/(?P<filename>\S+)$', 'polls.views.showcode'),
    (r'^polls/(?P<poll_id>\d+)/$', 'polls.views.detail'),
    (r'^polls/(?P<poll_id>\d+)/results/$', 'polls.views.results'),
    (r'^polls/(?P<poll_id>\d+)/vote/$', 'polls.views.vote'),
    # URL shortener
    (r'^createShortUrl/$', 'polls.views.newShortUrl'),
    (r'^u/(?P<code>[A-Za-z0-9]+)', 'polls.views.lookupShortUrl'),
    # admin stuff
    (r'^admin/', include(admin.site.urls)),
)
