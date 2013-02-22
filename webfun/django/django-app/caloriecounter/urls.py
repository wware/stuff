from django.conf.urls.defaults import patterns, include, url

import settings

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
    ( r'^static/(?P<path>.*)$',
      'django.views.static.serve',
      { 'document_root': settings.STATIC_ROOT } ),

    # Examples:
    # url(r'^$', 'caloriecounter.views.home', name='home'),
    # url(r'^caloriecounter/', include('caloriecounter.foo.urls')),

    url(r'^$', 'caloriecounter.views.index', name='index'),
    url(r'^newfood$', 'caloriecounter.views.newfood', name='newfood'),
    url(r'^newentry$', 'caloriecounter.views.newentry', name='newentry'),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    # url(r'^admin/', include(admin.site.urls)),
)
